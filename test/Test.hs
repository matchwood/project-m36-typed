module Main where


import RIO
import qualified RIO.List as L
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import ProjectM36.Typed
import ProjectM36.Typed.DB.Types
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Arbitrary as SOP
import Test.QuickCheck as QC

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $ [
    testCaseSteps "insertRecord" unit_insertRecord
  ]


data AppEnv db = AppEnv LogFunc (DbConnection db)

instance HasDbConnection (AppEnv) db where
  dbConnectionL = lens (\(AppEnv _ c) -> c) (\(AppEnv l _) db -> AppEnv l db)


instance HasLogFunc (AppEnv db) where
  logFuncL = lens (\(AppEnv l _) -> l) (\(AppEnv _ db) l -> AppEnv l db)

unit_insertRecord :: (String -> IO ()) -> Assertion
unit_insertRecord step = do
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogUseTime True logOptions
  withLogFunc logOptions' $ \lf -> do
    step "Connecting to db"
    eRes <-  connectProjectM36T lf (InProcessConnectionInfo NoPersistence emptyNotificationCallback []) dbSchema
    conn <- either (throwIO ) pure eRes
    runRIO (AppEnv lf conn) $ do
      (ps :: [PhoneNumber]) <- liftIO $ QC.generate $ sequence $ replicate 100 (arbitrary)
      eRa <- mapM (\p -> executeUpdateM $ insertT (Proxy :: Proxy "PhoneNumbers") p) ps
      void $ mapM (either throwIO pure) eRa

      ePs <- executeQueryM $ fetchT (Proxy :: Proxy "PhoneNumbers")
      ps1 <- either (throwIO ) pure ePs

      liftIO $ assertBool "Inserted numbers did not match fetched numbers" (L.sort ps == L.sort ps1)

      --pure $ testEquality (eRa) ps


data User = User
  { userFirstName :: Text
  , userLastName :: Text
  , userEmail :: Text
  , userDateOfBirth :: Maybe DateOfBirth
  --, userCredentials :: Credentials
  } deriving (Generic)



data Address = Address
  { addressLineOne :: Text
  , addressLineTwo :: Maybe Text
  , addressTown :: Maybe Text
  , addressCounty :: Maybe Text
  , addressCountry :: Maybe Text
  , addressPostcode :: Maybe Text
 -- , addressOwner :: RecordId User
  } deriving (Generic)


data PhoneNumber = PhoneNumber
  { phoneNumberNumber :: Text
  , phoneNumberComment :: Maybe Text
--, phoneNumberOwner :: RecordId User
  } deriving (Generic)

type AppSchema = InjectConstraints (
    (Define "Users" User)
  :& (Define "Addresses" Address)
  :& (Define "PhoneNumbers" PhoneNumber)
  )



deriving instance Eq User
deriving instance Ord User
deriving instance Show User
instance SOP.Generic User
instance SOP.HasDatatypeInfo User
instance Arbitrary User where arbitrary = SOP.garbitrary
instance AppRecordMeta User where
  type AppRecordName User = "User"
instance Tupleable User

deriving instance Eq Address
deriving instance Ord Address
deriving instance Show Address
instance SOP.Generic Address
instance SOP.HasDatatypeInfo Address
instance Arbitrary Address where arbitrary = SOP.garbitrary
instance AppRecordMeta Address where
  type AppRecordName Address = "Address"
instance Tupleable Address


deriving instance Eq PhoneNumber
deriving instance Ord PhoneNumber
deriving instance Show PhoneNumber
instance SOP.Generic PhoneNumber
instance SOP.HasDatatypeInfo PhoneNumber
instance Arbitrary PhoneNumber where arbitrary = SOP.garbitrary
instance AppRecordMeta PhoneNumber where
  type AppRecordName PhoneNumber = "PhoneNumber"
instance Tupleable PhoneNumber

--schema :: QSchema ( AppSchema)
--schema = mkSchema

dbSchema :: QDbSchema AppSchema
dbSchema = mkDbSchema