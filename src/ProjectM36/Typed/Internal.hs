{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints#-}

module ProjectM36.Typed.Internal (
  module ProjectM36.Typed.Internal,
  module ProjectM36.Typed.Schema

  ) where


import RIO
import qualified  RIO.Text as T
import qualified  RIO.Set as S

import ProjectM36.Base
import ProjectM36.Tupleable
import ProjectM36.Relation

import GHC.TypeLits
import qualified Generics.SOP as SOP
import Generics.SOP.Constraint
import Generics.SOP.NP
import Data.Kind (Type)

import Test.QuickCheck.Arbitrary

import ProjectM36.Typed.TypeFunctions
import ProjectM36.Typed.Generics
import ProjectM36.Typed.Schema
import ProjectM36.Typed.DB.Types


class (Typeable a, SOP.Generic a, SOP.HasDatatypeInfo a, Tupleable a, Arbitrary a, Eq a, Show a) => IsDbType a
instance (Typeable a, SOP.Generic a, SOP.HasDatatypeInfo a, Tupleable a, Arbitrary a, Eq a, Show a) => IsDbType a

class (Elem a (ExtractRelVars schema) ~ 'True, IsDbType a) => HasDbType schema a
instance (Elem a (ExtractRelVars schema) ~ 'True, IsDbType a) => HasDbType schema a




class (HasDbType schema a, KnownSymbol name, LookupRelVarType schema name ~ a) => HasNamedDbType schema name a
instance (HasDbType schema a, KnownSymbol name, LookupRelVarType schema name ~ a) => HasNamedDbType schema name a



class ToRelVarC schema a where
  toRelVarC :: proxy a -> RelVarC schema

instance (IsDbType a, HasNamedDbType schema name (DbRecord a)) => ToRelVarC schema (RelVarMappingDbRecord name a) where
  toRelVarC _ = RelVarCDb (Proxy :: Proxy name) (Proxy :: Proxy a)

instance (HasNamedDbType schema name a) => ToRelVarC schema (RelVarMappingRaw name a) where
  toRelVarC _ = RelVarCRaw (Proxy :: Proxy name) (Proxy :: Proxy a)




data RelVarC schema where
  RelVarCDb :: (IsDbType a, HasNamedDbType schema name (DbRecord a)) => Proxy name -> Proxy a -> RelVarC schema
  RelVarCRaw :: (HasNamedDbType schema name a) => Proxy name -> Proxy a -> RelVarC schema


type InbuiltAtomFunctions = '["count"]


type family KnownAtomFunctions schema :: [Symbol] where
  KnownAtomFunctions _ = InbuiltAtomFunctions

type family MapRequiresFields xs where
   MapRequiresFields '[]       = '[]
   MapRequiresFields (x ': xs) = RequiresFields x ': MapRequiresFields xs


type family MapAvailableFields xs where
   MapAvailableFields '[]       = '[]
   MapAvailableFields (x ': xs) = AvailableFields x ': MapAvailableFields xs



class ToRelationalExpr a where
  type RelationalExprT a
  type AvailableFields a :: [Symbol]
  type AvailableFields a = '[]
  type RequiresFields a :: [Symbol]
  type RequiresFields a = '[]

  toRelationalExpr :: proxy a ->  RelationalExprT a



type HasRequiredFields (instanceDetails :: ErrorMessage) (required :: [Symbol]) (available :: [Symbol]) = IfOrErr (AllSecondInFirst available required) (() :: Constraint) (('Text "Couldn't find all required fields " ':$$:
  'ShowType required ':$$:
  'Text "in the available fields " ':$$:
  'ShowType available ':$$:
  'Text "when resolving the constraints for " ':<>: instanceDetails
  ))

type MakeInstanceErrorMessage instanceT t =
  'Text "instance of " ':<>: 'ShowType instanceT ':<>:
  'Text " for type" ':$$: 'ShowType t

class (RelationalExprT a ~ expected, ToRelationalExpr a) => IsRelationalExprResult expected a
instance (RelationalExprT a ~ expected, ToRelationalExpr a) => IsRelationalExprResult expected a




instance ToRelationalExpr (ExistingRelation 'False) where
  type RelationalExprT (ExistingRelation 'False) = RelationalExpr
  toRelationalExpr _ = ExistingRelation relationFalse

instance ToRelationalExpr (ExistingRelation 'True) where
  type RelationalExprT (ExistingRelation 'True) = RelationalExpr
  toRelationalExpr _ = ExistingRelation relationTrue


instance (ToRelationalExpr rel1, ToRelationalExpr rel2, RelationalExprT rel1 ~ RelationalExpr, RelationalExprT rel2 ~ RelationalExpr) => ToRelationalExpr (NotEquals rel1 rel2) where
  type RelationalExprT (NotEquals rel1 rel2) = RelationalExpr
  toRelationalExpr _ = NotEquals (toRelationalExpr (Proxy :: Proxy rel1)) (toRelationalExpr (Proxy :: Proxy rel2))

instance (  ToRelationalExpr rel
          , RelationalExprT rel ~ RelationalExpr
          , All KnownSymbol fields
          , HasRequiredFields (MakeInstanceErrorMessage "ToRelationalExpr" (Project fields rel)) fields (AvailableFields rel)

          ) => ToRelationalExpr (Project fields rel) where
  type RelationalExprT (Project fields rel) = RelationalExpr
  type AvailableFields (Project fields rel) = fields
  toRelationalExpr _ = Project (AttributeNames . S.fromList . showSymbols $ (Proxy :: Proxy fields)) (toRelationalExpr (Proxy :: Proxy rel))



instance (  ToRelationalExpr rel
          , RelationalExprT rel ~ RelationalExpr
          , KnownSymbol old
          , KnownSymbol new
          , HasRequiredFields (MakeInstanceErrorMessage "ToRelationalExpr" (Rename old new rel)) '[old] (AvailableFields rel)
          ) => ToRelationalExpr (Rename old new rel) where
  type RelationalExprT (Rename old new rel) = RelationalExpr
  type AvailableFields (Rename old new rel) = new ': (RemoveTypes '[old] (AvailableFields rel))
  toRelationalExpr _ = Rename (showSymbol (Proxy :: Proxy old)) (showSymbol (Proxy :: Proxy new)) (toRelationalExpr (Proxy :: Proxy rel))



instance ( ToRelationalExpr e
         , RelationalExprT e ~ (ExtendTupleExprBase ())
         , ToRelationalExpr rel
         , RelationalExprT rel ~ (RelationalExpr)
         , HasRequiredFields (MakeInstanceErrorMessage "ToRelationalExpr" (Extend e rel)) (RequiresFields e) (AvailableFields rel)
    ) => ToRelationalExpr (Extend e rel) where
  type RelationalExprT (Extend e rel) = RelationalExpr
  type AvailableFields (Extend e rel) = AvailableFields e
  toRelationalExpr _ = Extend (toRelationalExpr (Proxy :: Proxy e)) (toRelationalExpr (Proxy :: Proxy rel))


instance (ToRelationalExpr rel, RelationalExprT rel ~ AtomExprBase (), KnownSymbol name) => ToRelationalExpr (AttributeExtendTupleExpr name rel) where
  type RelationalExprT (AttributeExtendTupleExpr name rel) = ExtendTupleExprBase ()
  type AvailableFields (AttributeExtendTupleExpr name rel) = '[name]
  type RequiresFields (AttributeExtendTupleExpr name rel) = RequiresFields rel
  toRelationalExpr _ = AttributeExtendTupleExpr (showSymbol (Proxy :: Proxy name)) (toRelationalExpr (Proxy :: Proxy rel))

instance (KnownSymbol name) => ToRelationalExpr (AttributeAtomExpr name) where
  type RelationalExprT (AttributeAtomExpr name) = AtomExprBase ()
  type RequiresFields (AttributeAtomExpr name) = '[name]
  toRelationalExpr _ = AttributeAtomExpr (showSymbol (Proxy :: Proxy name))

-- @todo we need to pass schema down through everything (or AtomFunction names up?) to do the Atom Function Name check this properly
instance (KnownSymbol name,  ToRelationalExpr xs, RelationalExprT xs ~ [AtomExprBase ()], Elem name InbuiltAtomFunctions ~ 'True) => ToRelationalExpr (FunctionAtomExpr name xs) where
  type RelationalExprT (FunctionAtomExpr name xs) = AtomExprBase ()
  type RequiresFields (FunctionAtomExpr name xs) = (RequiresFields xs)
  toRelationalExpr _ = FunctionAtomExpr (showSymbol (Proxy :: Proxy name)) (toRelationalExpr (Proxy :: Proxy xs)) ()


instance forall a xs. (ToRelationalExpr a, All ToRelationalExpr xs, All (IsRelationalExprResult (RelationalExprT a)) xs) => ToRelationalExpr (a ': xs) where
  type RelationalExprT (a ': xs) = [RelationalExprT a]
  type RequiresFields (a ': xs) = Concat (MapRequiresFields (a ': xs))
  type AvailableFields (a ': xs) = Concat (MapAvailableFields (a ': xs))

  toRelationalExpr _ = cfoldMap_NP (Proxy :: Proxy (IsRelationalExprResult (RelationalExprT a))) (pure . toRelationalExpr)  (pure_NP Proxy :: NP Proxy (a ': xs))


instance (ToRelationalExpr rel, RelationalExprT rel ~ RelationalExpr) => ToRelationalExpr (RelationAtomExpr rel) where
  type RelationalExprT (RelationAtomExpr rel) = AtomExprBase ()
  toRelationalExpr _ = RelationAtomExpr (toRelationalExpr (Proxy :: Proxy rel))




instance (KnownSymbol name) => ToRelationalExpr (Define name (a :: Type)) where
  type RelationalExprT (Define name a) = RelationalExpr
  type AvailableFields (Define name a) = ExtractFieldNames a
  toRelationalExpr _ = RelationVariable (showSymbol (Proxy :: Proxy name)) ()






class ToDatabaseContext a where
  toDatabaseContext :: Proxy a -> [DatabaseContextExpr]


instance (ToDatabaseContext a, ToDatabaseContext b) => ToDatabaseContext (a :& b) where
  toDatabaseContext _ = toDatabaseContext (Proxy :: Proxy a) ++ toDatabaseContext (Proxy :: Proxy b)

instance (Tupleable a, KnownSymbol sym) => ToDatabaseContext (Define sym a) where
  toDatabaseContext _ = [toDefineExpr (Proxy :: Proxy a) (T.pack $ symbolVal (Proxy :: Proxy sym))]



instance (ToDatabaseContext a, All ToDatabaseContext (ExpandApply xs a)) => ToDatabaseContext (a :$ xs) where
  toDatabaseContext _ = toDatabaseContext (Proxy :: Proxy a) ++ cfoldMap_NP (Proxy :: Proxy ToDatabaseContext) toDatabaseContext ((pure_NP Proxy ) :: NP Proxy (ExpandApply xs a))





instance (
    ToRelationalExpr rel1
  , ToRelationalExpr rel2
  , RelationalExprT rel1 ~ RelationalExpr
  , RelationalExprT rel2 ~ RelationalExpr
  , KnownSymbol n
  , HasRequiredFields (MakeInstanceErrorMessage "ToDatabaseContext" (AddInclusionDependency n rel1 rel2))  (AvailableFields (rel1)) (AvailableFields rel2)
  ) => ToDatabaseContext (AddInclusionDependency n rel1 rel2) where
  toDatabaseContext _ = [AddInclusionDependency (showSymbol $ (Proxy :: Proxy n)) $ InclusionDependency (toRelationalExpr (Proxy :: Proxy rel1)) (toRelationalExpr (Proxy :: Proxy rel2))]



type UniqueConstraintNamed name fields relvar = AddInclusionDependency name  (NotEquals (CountExpr relvar) (CountExpr (Project fields relvar)) ) (ExistingRelation 'False)


type CountExpr relvar = Project '["projectA"] (Extend (
   AttributeExtendTupleExpr "projectA"
  (FunctionAtomExpr "count" '[(AttributeAtomExpr "projectB")])
  )
  (
    Extend
      (AttributeExtendTupleExpr "projectB" (RelationAtomExpr relvar))
      (ExistingRelation 'True)
    )
  )

type MkUniqueConstraintName n fields = AppendSymbol (IntercalateSymbol "_" (n ': fields)) "_ukey"

instance (ToDatabaseContext (UniqueConstraintNamed (MkUniqueConstraintName n fields) fields (Define n a))) => ToDatabaseContext (UniqueConstraint fields (Define n a)) where
  toDatabaseContext _ = toDatabaseContext (Proxy :: Proxy (UniqueConstraintNamed (MkUniqueConstraintName n fields) fields (Define n a)))


type ForeignConstraintNamed name relvarA fieldsA relvarB fieldsB = AddInclusionDependency name  (RenameIfNecessary (Project fieldsA relvarA) (Zip fieldsA fieldsB)) (Project fieldsB relvarB)

type MkForeignConstraintName nameA fieldsA nameB fieldsB = AppendSymbol (IntercalateSymbol "_" (Union (nameA ': fieldsA) (nameB ': fieldsB))) "_fkey"


instance (ToDatabaseContext (ForeignConstraintNamed (MkForeignConstraintName nameA fieldsA nameB fieldsB) (Define nameA relvarA) fieldsA (Define nameB relvarB) fieldsB)) => ToDatabaseContext (ForeignConstraint fieldsA (Define nameB relvarB) fieldsB (Define nameA relvarA)) where
  toDatabaseContext _ = toDatabaseContext (Proxy :: Proxy (ForeignConstraintNamed (MkForeignConstraintName nameA fieldsA nameB fieldsB) (Define nameA relvarA) fieldsA (Define nameB relvarB) fieldsB))

type family RenameIfNecessary expr (fields :: [Tup Symbol Symbol]) where
  RenameIfNecessary expr '[] = expr
  RenameIfNecessary expr ('Tup e e ': rest) = RenameIfNecessary expr rest
  RenameIfNecessary expr ('Tup existing expected ': rest) = RenameIfNecessary (Rename existing expected expr) rest





class ToDatabaseSchemaContext a where
  toDatabaseSchemaContext :: Proxy a -> [DatabaseContextExpr]


instance (ToDatabaseSchemaContext a, ToDatabaseSchemaContext b) => ToDatabaseSchemaContext (a :& b) where
  toDatabaseSchemaContext _ = toDatabaseSchemaContext (Proxy :: Proxy a) ++ toDatabaseSchemaContext (Proxy :: Proxy b)

instance (ToDatabaseContext (Define sym a)) => ToDatabaseSchemaContext (Define sym a) where
  toDatabaseSchemaContext = toDatabaseContext

instance (ToDatabaseSchemaContext a, All ToDatabaseSchemaContext (ExpandApply xs a)) => ToDatabaseSchemaContext (a :$ xs) where
  toDatabaseSchemaContext _ = toDatabaseSchemaContext (Proxy :: Proxy a) ++ cfoldMap_NP (Proxy :: Proxy ToDatabaseSchemaContext) toDatabaseSchemaContext ((pure_NP Proxy ) :: NP Proxy (ExpandApply xs a))

instance (ToDatabaseContext (AddInclusionDependency n rel1 rel2)) => ToDatabaseSchemaContext (AddInclusionDependency n rel1 rel2) where
  toDatabaseSchemaContext = toDatabaseContext


instance (ToDatabaseContext (UniqueConstraint a b )) => ToDatabaseSchemaContext (UniqueConstraint a b) where
  toDatabaseSchemaContext = toDatabaseContext

instance (ToDatabaseContext (ForeignConstraint a b c d)) => ToDatabaseSchemaContext (ForeignConstraint a b c d) where
  toDatabaseSchemaContext = toDatabaseContext









type IsValidDbSchema db = (
     All (HasDbType db) (ExtractRelVars db)
   , All (ToRelVarC db) (ExtractRelVarMappings db)
   , UniqueElementsWithErr (ExtractRelVarNames db) ~ 'True
   , ToDatabaseSchemaContext db
   )




data QDbSchema db where
  QDbSchema :: IsValidDbSchema db =>  {unwrapQDbSchema2 :: Proxy db} -> QDbSchema db


mkDbSchema :: IsValidDbSchema a => QDbSchema a
mkDbSchema = QDbSchema Proxy
