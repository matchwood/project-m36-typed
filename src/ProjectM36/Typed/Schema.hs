{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ProjectM36.Typed.Schema where

import RIO

import qualified Data.Type.Map as TM

import Generics.SOP.NP
--import Generics.SOP.BasicFunctors

import Data.Proxy(Proxy(..))
import qualified  RIO.Text as T
import Generics.SOP.Constraint

import ProjectM36.Typed.TypeFunctions
import ProjectM36.Typed.Generics
import ProjectM36.Typed.DB.Types
import Generics.SOP.Dict
import qualified Generics.SOP as SOP
import Test.QuickCheck.Arbitrary

import GHC.TypeLits

class (KnownSymbol (AppRecordName a ), Generic a) => AppRecordMeta a where
  type AppRecordName a :: Symbol

instance (AppRecordMeta a) => AppRecordMeta (DbRecord a) where
  type AppRecordName (DbRecord a) = AppRecordName a


showAppRecordName :: forall p a. (AppRecordMeta a) => p a -> T.Text
showAppRecordName _ = showSymbol (Proxy :: Proxy (AppRecordName a))


class (Typeable a, AppRecordMeta a, SOP.Generic a, SOP.HasDatatypeInfo a, Arbitrary a, Eq a, Show a) => IsAppType a
instance (Typeable a, AppRecordMeta a, SOP.Generic a, SOP.HasDatatypeInfo a, Arbitrary a, Eq a, Show a) => IsAppType a


class (Elem a db ~ 'True, IsAppType a) => HasAppType db a
instance (Elem a db ~ 'True, IsAppType a) => HasAppType db a



-- @todo this type produces a nasty error message if the db types are not in AppRecordMeta. Ideally we would provide a custom error in this case but it seems to be impossible with the type applications we have (the issue is really in DbRecordMetaSymbols)


type IsValidAppSchema db = (
     All (HasAppType (ExtractRelVars db)) (ExtractRelVars db)
   , All (HasAppType (ExtractUniqueRelVarBaseTypes db)) (ExtractUniqueRelVarBaseTypes db)
   , UniqueElementsWithErr (ExtractRelVarNames db) ~ 'True
   )



data QSchema schema where
 QSchema :: IsValidAppSchema schema => Proxy schema -> QSchema schema







mkSchema :: IsValidAppSchema schema => QSchema schema
mkSchema = QSchema $ Proxy



instance WithCDictionary Typeable (QSchema schema) where
  type CDictionaryList Typeable (QSchema schema) = ExtractUniqueRelVarBaseTypes schema
  toCDictionary (QSchema _) = cliftA_NP (Proxy :: Proxy (HasAppType (ExtractUniqueRelVarBaseTypes schema))) (const Dict) (pure_NP Proxy  :: NP Proxy (ExtractUniqueRelVarBaseTypes schema))
  withCDictionary f (s@(QSchema _)) = f (toCDictionary s)



{- SCHEMA DSL-}



{- RELATIONAL -}

-- RelationalExprBase
data ExistingRelation a
data RelationVariable a
data Project (names :: [Symbol]) a
data NotEquals rel1 rel2
data Extend a b
data Rename (old :: Symbol) (new :: Symbol) rel

-- ExtendTupleExprBase
data AttributeExtendTupleExpr (name :: Symbol) a

-- AtomExprBase
data AttributeAtomExpr (name :: Symbol)
data FunctionAtomExpr (name :: Symbol) (a :: [*])
data RelationAtomExpr a



{- Types and overall structure -}


data a :& b
infixr 3 :&
data (a :: *) :$ (b :: [* -> *])
infixr 4 :$

data AddInclusionDependency (name :: Symbol) relEx1 relEx2


data NoOperation
data Define (sym :: Symbol) a
data UniqueConstraint (fields :: [Symbol]) relvar


data ForeignConstraint (fieldsA :: [Symbol]) relvarB (fieldsB :: [Symbol]) relvarA


type RelVarD a = Define (AppRecordName a) (DbRecord a)






type family InjectConstraints a where
  InjectConstraints a = InjectConstraintsBase a a

type family InjectConstraintsBase schema a where
  InjectConstraintsBase schema (a :& b) = InjectConstraintsBase schema a :& InjectConstraintsBase schema b
  InjectConstraintsBase schema (a :$ existingConstraints) = a :$ Union (DeriveConstraints schema a) existingConstraints
  InjectConstraintsBase schema (Define a (DbRecord b)) = (Define a (DbRecord b)) :$ DeriveConstraints schema (Define a (DbRecord b))
  InjectConstraintsBase schema a = a


type family DeriveConstraints schema a :: [* -> *] where
  DeriveConstraints schema a = Union (DeriveUniqueConstraints schema a) (DeriveForeignConstraints schema a)


type family DeriveUniqueConstraints schema a :: [* -> *] where
  DeriveUniqueConstraints _ (Define _ (DbRecord a)) = '[UniqueConstraint '["dbRecordId"]]
  DeriveUniqueConstraints _ (Define _ a) = '[]

type family DeriveForeignConstraints schema a :: [* -> *] where
  DeriveForeignConstraints schema (Define n (DbRecord a)) = MakeForeignConstraints schema (ExtractFields a)
  DeriveForeignConstraints _ (Define _ a) = '[]

-- iterate over fields and then find target type name
type family MakeForeignConstraints schema (fields :: [Field Symbol *]) :: [* -> *] where
  MakeForeignConstraints _ '[] = '[]
  -- ignore own recordId
  MakeForeignConstraints schema (('Field "dbRecordId" _) ': xs) = MakeForeignConstraints schema xs
  -- simplest - a single record id
  MakeForeignConstraints schema (('Field n (RecordId a)) ': xs) =
    (ForeignConstraint '[n] (Define (LookupUniqueRelVarName schema (DbRecord a)) (DbRecord a)) '["dbRecordId"]) ': (MakeForeignConstraints schema xs)
  -- @ todo add logic for maybe / list keys
  MakeForeignConstraints schema (_ ': xs) = MakeForeignConstraints schema xs




type family IsWrappedByDbRecord db a :: Bool where
  IsWrappedByDbRecord (a :& b) c = Or (IsWrappedByDbRecord a c) (IsWrappedByDbRecord b c)
  IsWrappedByDbRecord (a :$ _) c = IsWrappedByDbRecord a c
  IsWrappedByDbRecord (Define _ (DbRecord a)) a  = 'True
  IsWrappedByDbRecord (Define _ a) a = 'False
  IsWrappedByDbRecord _ _ = 'False


type ExtractUniqueRelVarBaseTypes a = Nub (ExtractRelVarBaseTypes a)


type family ExtractRelVarBaseTypes a = (res :: [*]) where
  ExtractRelVarBaseTypes (a :& b) = Union (ExtractRelVarBaseTypes a) (ExtractRelVarBaseTypes b)
  ExtractRelVarBaseTypes (a :$ _) = ExtractRelVarBaseTypes a
  ExtractRelVarBaseTypes (Define _ (DbRecord a)) = '[a]
  ExtractRelVarBaseTypes (Define _ a) = '[a]


type family ExtractRelVars a = (res :: [*]) where
  ExtractRelVars a = TMValues (ToRelVarMap a)

type family ExtractRelVarNames a = (res :: [Symbol]) where
  ExtractRelVarNames a = TMKeys (ToRelVarMap a)

data RelVarMappingDbRecord (name :: Symbol) (a :: *)
data RelVarMappingRaw (name :: Symbol) (a :: *)

type family ExtractRelVarMappings a where
  ExtractRelVarMappings a = ToRelVarMappings (ToRelVarMap a)

type family ToRelVarMappings (xs :: [TM.Mapping Symbol *]) :: [*] where
  ToRelVarMappings '[] = '[]
  ToRelVarMappings ((s 'TM.:-> (DbRecord a)) ':xs) = (RelVarMappingDbRecord s a) ': (ToRelVarMappings xs)
  ToRelVarMappings ((s 'TM.:-> a) ':xs) = (RelVarMappingRaw s a) ': (ToRelVarMappings xs)



type family ToRelVarMap a = (res :: [TM.Mapping Symbol *])  where
  ToRelVarMap (a :& b) = Union (ToRelVarMap a) (ToRelVarMap b)
  ToRelVarMap (a :$ _) = ToRelVarMap a
  ToRelVarMap (Define s a) = '[s 'TM.:-> a]

type family FromMaybe (a :: Maybe k) where
  FromMaybe ('Just a) = a

type family LookupUniqueRelVarName schema a :: Symbol where
  LookupUniqueRelVarName schema a = SingleValueFromListOrErr (LookupTMKeys (ToRelVarMap schema) a) (
    'Text "Expected to find exactly one relvar name for the type " ':<>: 'ShowType a ':<>: 'Text " but found "
    )


type family LookupRelVarType schema a where
  LookupRelVarType schema a = FromMaybe (LookupTMValue (ToRelVarMap schema) a)


type family TMKeys (a :: [TM.Mapping Symbol *]) :: [Symbol] where
  TMKeys '[] = '[]
  TMKeys ((k 'TM.:-> _) ': xs) = k ': (TMKeys xs)

type family TMValues (a :: [TM.Mapping Symbol *]) :: [*] where
  TMValues '[] = '[]
  TMValues ((_ 'TM.:-> v) ': xs) = v ': (TMValues xs)

type family LookupTMKey (m :: [TM.Mapping Symbol *]) (a :: *) :: (Maybe Symbol) where
  LookupTMKey ((k 'TM.:-> v) ': xs) v = 'Just k
  LookupTMKey ((_ 'TM.:-> x) ': xs) v = LookupTMKey xs v
  LookupTMKey _ _ = 'Nothing

type family LookupTMValue (m :: [TM.Mapping Symbol *]) (a :: Symbol) :: (Maybe *) where
  LookupTMValue ((k 'TM.:-> v) ': xs) k = 'Just v
  LookupTMValue ((_ 'TM.:-> x) ': xs) k = LookupTMValue xs k
  LookupTMValue _ _ = 'Nothing

type family LookupTMKeys (m :: [TM.Mapping Symbol *]) (a :: *) :: ([Symbol]) where
  LookupTMKeys ((k 'TM.:-> v) ': xs) v = Union '[k] (LookupTMKeys xs v)
  LookupTMKeys ((_ 'TM.:-> x) ': xs) v = LookupTMKeys xs v
  LookupTMKeys _ _ = '[]
