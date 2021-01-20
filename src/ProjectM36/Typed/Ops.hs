
module ProjectM36.Typed.Ops where

import RIO
import qualified RIO.List as L

import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Tupleable (Tupleable(..), toInsertExpr)

import Control.Monad.Except

import ProjectM36.Typed.Internal
import ProjectM36.Typed.Execute
import ProjectM36.Typed.Generics

type SchemaOpM env m = (MonadError DbErrorQ m, MonadIO m, MonadReader env m)

createSchema :: forall env db. (HasLogFunc env) => SessionId -> Connection -> QDbSchema db -> RIO env (Either DbErrorQ (DbConnection db))
createSchema sid conn sc@(QDbSchema schemaP) = do
  mainRes <- withTransactionUsingQ (sid, conn) UnionMergeStrategy $ runExceptT $ do
    logInfo $ "Creating schema"
    let operations = [
            ("Automated generation of database context from schema", sortByWeight $ toDatabaseSchemaContext schemaP)
          ]
    warnDuplicates operations
    void $ mapM runOperation operations
    return $ DbConnection  sid conn sc
  when (isRight mainRes) $ logInfo "Schema created successfully"
  return mainRes

  where
    warnDuplicates :: (SchemaOpM env m) => [(Text, [DatabaseContextExpr])] -> m ()
    warnDuplicates ops = do
      case findDuplicates $ concat $ map snd ops  of
        [] -> pure ()
        ds -> do
          logWarn "Duplicates detected in schema creation operations: "
          logWarn $ displayShow ds
    findDuplicates :: (Eq a) => [a] -> [a]
    findDuplicates [] = []
    findDuplicates (x:xs) =
      if x `elem` xs
        then x : findDuplicates xs
        else findDuplicates xs
    sortByWeight :: [DatabaseContextExpr] -> [DatabaseContextExpr]
    sortByWeight xs = map fst $ L.sortBy (comparing snd) $ map (\e -> (e, dceWeight e)) xs
    dceWeight :: DatabaseContextExpr -> Int
    dceWeight (Define _ _) = 5
    dceWeight (AddInclusionDependency _ _ ) = 10
    dceWeight _ = 15
    runOperation :: (SchemaOpM env m) => (Text, [DatabaseContextExpr]) -> m ()
    runOperation (n, !exprs) = do
      logInfo $ "Running schema operation: " <> displayShow n
      void $ mapM runContextExpr exprs

    runContextExpr :: (SchemaOpM env m) => DatabaseContextExpr -> m ()
    runContextExpr e = do
      logInfo $ "Executing database context expr"
      logInfo $ displayShow e
      handleUnexpectedError $ liftIO $ executeDatabaseContextExpr sid conn e

    handleUnexpectedError :: (SchemaOpM env m) => m (Either RelationalError ()) -> m ()
    handleUnexpectedError act = do
      err <- act
      case err of
        Left e ->
          if (isExpectedError e)
            then do
              logInfo $ "Expected error: " <> displayShow e
              pure ()
            else throwError . toDbErrorQ $ e
        Right _ -> pure ()
  {-    handleUnexpectedErrors :: (SchemaOpM env m) => m [Either RelationalError ()] -> m ()
      handleUnexpectedErrors act = do
        errs <- act
        let (expected, unexpected) = L.partition isExpectedError (lefts errs)
        when (not . null $ expected) (void $ mapM (logInfo . displayShow) expected)

        case NE.nonEmpty $ unexpected of
         Nothing -> return ()
         Just xs -> throwError . toDbErrorQ $ NE.map toDbErrorQ xs-}

    isExpectedError :: RelationalError -> Bool
    isExpectedError (RelVarAlreadyDefinedError _) = True
    isExpectedError (InclusionDependencyNameInUseError _) = True
    isExpectedError _ = False





insertT :: forall db name a . (HasNamedDbType db name a) => Proxy name -> a -> UpdateM db a
insertT pName a = do
  e <- liftEitherQ $ toInsertExpr [a] (showSymbol pName)
  throwQ $ executeUpdate e
  return a



insertBulkT :: forall db name a . (HasNamedDbType db name a) => Proxy name -> [a] -> UpdateM db [a]
insertBulkT pName as = do
  e <- liftEitherQ $ toInsertExpr as (showSymbol pName)
  throwQ $ executeUpdate e
  return as



fetchT :: forall db name a . (HasNamedDbType db name a) => Proxy name -> QueryM db [a]
fetchT pName = do
  res <- throwQ $ executeQuery (RelationVariable (showSymbol pName) ())
  liftEitherQ $ sequence $ map fromTuple (relationTuples res)

