{-# LANGUAGE DeriveAnyClass #-}
module ProjectM36.Typed.DB.Types where

import RIO
import qualified RIO.Time as Time

import System.Random

import Data.Binary
import Data.Binary.Orphans()

import ProjectM36.Typed.Types
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Arbitrary as SOP

import Test.QuickCheck.Arbitrary
import ProjectM36.Typed.Gen

type Date = Time.UTCTime

newtype RecordSoftDeleted = RecordSoftDeleted Bool
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Binary, Arbitrary)

newtype RecordCreated = RecordCreated Date
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Binary, Arbitrary)

newtype RecordLastModified = RecordLastModified (Maybe Date)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Binary, Arbitrary)

data DbRecord a = DbRecord {
    dbRecordRecord :: a
  , dbRecordId :: RecordId a
  , dbRecordCreated :: RecordCreated
  , dbRecordLastModified :: RecordLastModified
  , dbRecordDeleted :: RecordSoftDeleted
 -- , dbRecordLogs :: IxSet.IxSet DbRecordLogIxs DbRecordLog
  , dbRecordETag :: ETag
  } deriving (Eq, Show, Generic)

instance SOP.Generic (DbRecord a)
instance SOP.HasDatatypeInfo (DbRecord a)
instance (Arbitrary a) => Arbitrary (DbRecord a) where arbitrary = SOP.garbitrary




data DbDeletedRecord a = DbDeletedRecord {
    dbDeletedRecordId :: RecordId a
 -- , dbDeletedRecordLogs :: IxSet.IxSet DbRecordLogIxs DbRecordLog
  }

-- record logs
data DbRecordLogEvent =
    DbInsert
  | BBReplace
  | DbPatch
  | DbSoftDelete
  | DbHardDelete

data DbRecordLog = DbRecordLog {
    dbRecordLogTime :: Date
  , dbRecordLogEvent :: DbRecordLogEvent
  }


data RecordId a where
  RecordId :: SafeId -> RecordId a
  deriving (Generic, Eq, Show)
  deriving anyclass (Binary, NFData)


instance Arbitrary (RecordId a) where
  arbitrary = do
    g <- gFromGen
    pure . RecordId . fst . random $ g


safeIdFromRecordId :: RecordId a -> SafeId
safeIdFromRecordId (RecordId s) = s

data Record a = Record {
    recordRecord :: a
  , recordId :: RecordId a
  , recordCreated :: RecordCreated
  , recordLastModified :: RecordLastModified
  , recordDeleted :: RecordSoftDeleted
  , recordETag :: ETag
  }

dbRecordToRecord :: DbRecord a -> Record a
dbRecordToRecord DbRecord{..} = Record {
    recordRecord = dbRecordRecord
  , recordId = dbRecordId
  , recordCreated = dbRecordCreated
  , recordLastModified = dbRecordLastModified
  , recordDeleted = dbRecordDeleted
  , recordETag = dbRecordETag
  }

data DbError =
    DbErrorNotFound Text
  deriving (Show)


type DbGen = StdGen


-- USEFUL TYPES FOR STORING DATA
-- @todo extend this to be a newtype with length constrained using a smart constructor?
type ShortText = Text


data Gender =
    GenderFemale
  | GenderMale
  | GenderUnspecified
  deriving (Generic, Eq, Show)

type DateOfBirth = Time.Day
