
module ProjectM36.Typed.Wrapped.Helper where

import RIO
import qualified  RIO.Text as T
import qualified  RIO.Set as S

import ProjectM36.Base
import ProjectM36.Client


toUniqueKeyName :: RelVarName -> [AttributeName] -> T.Text
toUniqueKeyName rName as = (rName <> "_" <> T.intercalate "_" as <> "_key")


databaseContextExprForUniqueKeyWithUniqueName :: RelVarName -> [AttributeName] -> DatabaseContextExpr
databaseContextExprForUniqueKeyWithUniqueName rName as =
  AddInclusionDependency (toUniqueKeyName rName as) $ inclusionDependencyForKey (AttributeNames (S.fromList as)) (RelationVariable rName ())