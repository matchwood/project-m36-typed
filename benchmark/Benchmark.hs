
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import RIO
import Criterion.Main
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Arbitrary as SOP
import Test.QuickCheck as QC

import ProjectM36.Typed
import ProjectM36.Typed.DB.Types




main :: IO ()
main = do

  let conf = defaultConfig
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogUseTime True logOptions
  withLogFunc logOptions' $ \lf -> do
    defaultMainWith conf $
       [
       env (openDB lf) $ \db ->
         bench "insertRecord" $ whnfIO (runInsertUser lf db)
       ]


data AppEnv db = AppEnv LogFunc (DbConnection db)
instance NFData (DbConnection db) where
  rnf _ = ()

instance HasDbConnection (AppEnv) db where
  dbConnectionL = lens (\(AppEnv _ c) -> c) (\(AppEnv l _) db -> AppEnv l db)


instance HasLogFunc (AppEnv db) where
  logFuncL = lens (\(AppEnv l _) -> l) (\(AppEnv _ db) l -> AppEnv l db)

runInsertUser :: LogFunc -> DbConnection AppSchema -> IO User
runInsertUser lf db = runRIO (AppEnv lf db) $ do
  u <- liftIO $ QC.generate $ arbitrary
  er <- executeUpdateM $ insertT (Proxy :: Proxy "Users") u
  either (throwIO ) pure er


openDB :: LogFunc -> IO (DbConnection AppSchema)
openDB lf = do
  eRes <- connectProjectM36T lf (InProcessConnectionInfo NoPersistence emptyNotificationCallback []) dbSchema
  db <- either (throwIO ) pure eRes
  runRIO (AppEnv lf db) $ do
    (us :: [User]) <- liftIO $ QC.generate $ sequence $ replicate 100000 (arbitrary)
    er <- executeUpdateM $ insertBulkT (Proxy :: Proxy "Users") us
    _ <- either (throwIO ) pure er
    pure ()
  pure db


data User = User
  { userFirstName :: Text
  , userLastName :: Text
  , userEmail :: Text
  , userDateOfBirth :: Maybe DateOfBirth
  } deriving (Generic)

deriving instance Eq User
deriving instance Ord User
deriving instance Show User
instance SOP.Generic User
instance SOP.HasDatatypeInfo User
instance Arbitrary User where arbitrary = SOP.garbitrary
instance AppRecordMeta User where
  type AppRecordName User = "User"
instance Tupleable User

type AppSchema = (
    (Define "Users" User)
  )

dbSchema :: QDbSchema AppSchema
dbSchema = mkDbSchema
