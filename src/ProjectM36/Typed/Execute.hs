{-# LANGUAGE UndecidableInstances #-}

module ProjectM36.Typed.Execute where

import RIO
import qualified RIO.Time as T


import ProjectM36.Base
import ProjectM36.Client

import Control.Monad.Except


import qualified Data.List.NonEmpty as NE
import ProjectM36.Typed.Internal

import Quartz.Common.Gen

data DbConnection db = DbConnection {
  dbSession :: SessionId,
  dbConnection :: Connection,
  dbConnSchema :: (QDbSchema db)
  }

{- ERRORS -}
data DbErrorQ =
    DbQRelError RelationalError
  | DbQConnError ConnectionError
  | DbQErrorRollbackError RelationalError DbErrorQ
  | DbQErrorList (NE.NonEmpty DbErrorQ)
  deriving (Eq, Show)


class ToDbErrorQ a where
  toDbErrorQ :: a -> DbErrorQ

instance ToDbErrorQ DbErrorQ where
  toDbErrorQ = id
instance ToDbErrorQ RelationalError where
  toDbErrorQ = DbQRelError
instance ToDbErrorQ ConnectionError where
  toDbErrorQ = DbQConnError
instance ToDbErrorQ (NE.NonEmpty DbErrorQ) where
  toDbErrorQ = DbQErrorList


data RollbackException = RollbackException RelationalError SomeException
  deriving (Show)
instance Exception RollbackException





{- low level wrapping functions for m36-}
withTransactionQ :: forall m erra errb a env.  (MonadUnliftIO m, ToDbErrorQ erra, ToDbErrorQ errb, MonadReader env m, HasLogFunc env) => SessionId -> Connection -> m (Either erra a) -> m (Either errb ()) -> m (Either DbErrorQ a)
withTransactionQ sessionId conn act successFunc = catchAny doAct doUnrecoverableRollback
  where
    doRollback :: (Show ex) => ex -> m (Either RelationalError ())
    doRollback ex = do
      logInfo ("attempting rollback after encountering error" <> displayShow ex)
      res <- liftIO $ rollback sessionId conn
      logInfo ("Rollback seems to have completed succesfully")
      return res

    doUnrecoverableRollback :: SomeException -> m (Either DbErrorQ a)
    doUnrecoverableRollback ex = do
      res <- doRollback ex
      case res of
        Left e -> throwIO $ RollbackException e ex
        Right _ -> throwIO ex

    doAct :: m (Either DbErrorQ a)
    doAct = do
      eErr <- act
      case eErr of
        Left err -> do
          rbRes <- doRollback $ toDbErrorQ err
          case rbRes of
            Left relE -> pure . Left $ DbQErrorRollbackError relE (toDbErrorQ err)
            Right _ -> pure . Left . toDbErrorQ $ err
        Right val -> do
            eIsDirty <- liftIO $ disconnectedTransactionIsDirty sessionId conn
            case eIsDirty of
              Left err -> pure . Left . toDbErrorQ $ err
              Right dirty ->
                if dirty then do
                  res <- successFunc
                  case res of
                    Left err -> pure . Left . toDbErrorQ $ err
                    Right _ -> pure (Right val)
                  else -- no updates executed, so don't create a commit
                  pure (Right val)


withTransactionUsingQ :: (MonadUnliftIO m, ToDbErrorQ err, MonadReader env m, HasLogFunc env) => (SessionId, Connection) -> MergeStrategy -> m (Either err a) -> m (Either DbErrorQ a)
withTransactionUsingQ (sess, conn) strat dbm = do
  eHeadName <- liftIO $ headName sess conn
  case eHeadName of
    Left err -> pure . Left . toDbErrorQ $ err
    Right hn -> do
      let successFunc = liftIO $ autoMergeToHead sess conn strat hn
      withTransactionQ sess conn dbm successFunc


relationTuples :: Relation -> [RelationTuple]
relationTuples (Relation _ ts) = asList ts

{- classes for interacting with the db-}



class (Monad m) => CanUpdateDb m where
  executeUpdate :: DatabaseContextExpr -> m (Either RelationalError ())

class (Monad m) => CanQueryDb m where
  executeQuery :: RelationalExpr -> m (Either RelationalError Relation)

class HasDbConnection a db where
  dbConnectionL :: Lens' (a db) (DbConnection db)


class HasCurrentTime env where
  getCurrentTimeL :: Lens' env T.UTCTime

class GetCurrentTimeM m where
  getCurrentTimeM :: m T.UTCTime







{- Concrete instances for writing db functions -}


newtype QueryM db a = QueryM {extractQueryM :: (forall env . (HasLogFunc (env db), HasDbConnection env db) =>  ExceptT DbErrorQ (RIO (env db)) a)}
 deriving (Functor)

instance Applicative (QueryM db) where
  pure a = QueryM (pure a)
  (<*>) (QueryM mf) (QueryM ma) = QueryM $ mf <*> ma

instance Monad (QueryM db) where
  (>>=) (QueryM ma) f = QueryM $ do
    a <- ma
    let (QueryM mb) = f a
    mb

instance MonadError DbErrorQ (QueryM db) where
  throwError e = QueryM $ throwError e
  catchError (QueryM a) f = QueryM $ catchError a (extractQueryM . f)


runIOInQueryM :: IO a -> QueryM db a
runIOInQueryM act = QueryM $ liftIO act

executeQueryM :: (HasLogFunc (env db), HasDbConnection env db, MonadReader (env db) m, MonadIO m) => QueryM db a -> m (Either DbErrorQ a)
executeQueryM (QueryM a) = do
  DbConnection{..} <- view dbConnectionL
  liftRIO $ runExceptT a

instance CanQueryDb (QueryM db) where
  executeQuery c = do
    DbConnection{..} <- QueryM $ view dbConnectionL
    runIOInQueryM $ executeRelationalExpr dbSession dbConnection c



data UpdateMState db = UpdateMState {
    updateMStateLogFunc :: LogFunc,
    updateMStateDbConnection :: DbConnection db,
    updateMStateCurrentTime :: T.UTCTime
  }

instance HasLogFunc (UpdateMState db) where
  logFuncL = lens updateMStateLogFunc (\x y -> x { updateMStateLogFunc = y })

instance HasDbConnection UpdateMState db where
  dbConnectionL = lens updateMStateDbConnection (\x y -> x { updateMStateDbConnection = y })

instance HasCurrentTime (UpdateMState db) where
  getCurrentTimeL = lens updateMStateCurrentTime (\x y -> x { updateMStateCurrentTime = y })


instance GetCurrentTimeM (UpdateM db) where
  getCurrentTimeM = UpdateM (view getCurrentTimeL)

liftLog ::  Utf8Builder -> UpdateM db ()
liftLog a = UpdateM $ logInfo a

mkUpdateMState :: (HasLogFunc (env db), HasDbConnection env db, MonadReader (env db) m, MonadIO m) => m (UpdateMState db)
mkUpdateMState = UpdateMState <$>
  (view logFuncL) <*>
  (view dbConnectionL) <*>
  T.getCurrentTime

instance RandomM (UpdateM db) where
  randomM = UpdateM $ lift  randomM


newtype UpdateM db a = UpdateM {extractUpdateM :: (forall env . (HasLogFunc (env db), HasCurrentTime (env db), HasDbConnection env db) =>  ExceptT DbErrorQ (GenT (RIO (env db))) a)}
 deriving (Functor)

instance Applicative (UpdateM db) where
  pure a = UpdateM (pure a)
  (<*>) (UpdateM mf) (UpdateM ma) = UpdateM $ mf <*> ma

instance Monad (UpdateM db) where
  (>>=) (UpdateM ma) f = UpdateM $ do
    a <- ma
    let (UpdateM mb) = f a
    mb

instance MonadError DbErrorQ (UpdateM db) where
  throwError e = UpdateM $ throwError e
  catchError (UpdateM a) f = UpdateM $ catchError a (extractUpdateM . f)




runIOInUpdateM :: IO a -> UpdateM db a
runIOInUpdateM act = UpdateM $ liftIO act

executeUpdateM :: (HasLogFunc (env db), HasDbConnection env db, MonadReader (env db) m, MonadIO m) => UpdateM db a -> m (Either DbErrorQ a)
executeUpdateM (UpdateM a) = do
  DbConnection{..} <- view dbConnectionL
  s <- mkUpdateMState
  runRIO s $ withTransactionUsingQ (dbSession, dbConnection) UnionMergeStrategy (runGenT . runExceptT $ a)



instance CanUpdateDb (UpdateM db) where
  executeUpdate c = do
    DbConnection{..} <- UpdateM $ view dbConnectionL
    runIOInUpdateM $ executeDatabaseContextExpr dbSession dbConnection c

instance CanQueryDb (UpdateM db) where
  executeQuery c = do
    DbConnection{..} <- UpdateM $ view dbConnectionL
    runIOInUpdateM $ executeRelationalExpr dbSession dbConnection c




{- utility functions for error handling-}

liftEitherQ :: (ToDbErrorQ e, MonadError DbErrorQ m) => Either e a -> m a
liftEitherQ = either (throwError . toDbErrorQ) pure

throwQ :: (ToDbErrorQ e, MonadError DbErrorQ m) => m (Either e a) -> m a
throwQ a = a >>= liftEitherQ

