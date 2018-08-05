{-# LANGUAGE UndecidableInstances #-}

module ProjectM36.Typed.TypeFunctions where

import RIO

import GHC.Generics
import GHC.TypeLits

import Quartz.Common.DB.Types
--import qualified GHC.Records  as R

{-
class HasFieldT r field

instance (R.HasField field r a, KnownSymbol field) => HasFieldT r field-}


-- @todo DbRecord in this filehere creates a dependency on Quartz  - also this should filter out the "dbRecordRecord" field, as that is not actually part of the tupleable

data Field name t = Field name t



type family Nub (xs :: [*]) :: [*] where
  Nub '[] = '[]
  Nub (x ': xs) = If (Elem x xs) (Nub xs) (x ': Nub xs)

type family ExtractFieldNames (r :: *) :: [Symbol] where

  ExtractFieldNames r = DoExtractFieldNames (ExtractFields r)

type family DoExtractFieldNames (r :: [Field Symbol *]) :: [Symbol] where
  DoExtractFieldNames '[] = '[]
  DoExtractFieldNames (('Field n _) ':xs) = Union '[n] (DoExtractFieldNames xs)



type family ExtractFields (r :: *):: [Field Symbol *] where
  ExtractFields (DbRecord a) = Union (ExtractFieldsG (Rep a)) (ExtractFieldsG (Rep (DbRecord a)))
  ExtractFields a = ExtractFieldsG (Rep a)


type family ExtractFieldsG (r :: * -> *) :: [Field Symbol *] where
  ExtractFieldsG (l :*: r)
    = Union (ExtractFieldsG l) (ExtractFieldsG r)
  ExtractFieldsG (S1 ('MetaSel ('Just name) _ _ _) (Rec0 t))
    = '[ 'Field name t]
  ExtractFieldsG (M1 _ m a)
    = ExtractFieldsG a
  ExtractFieldsG _
    = '[]


type family Elem (a :: k) (b :: [k]) :: Bool where
    Elem a '[] = 'False
    Elem a (a ': xs) = 'True
    Elem a (b ': xs) = Elem a xs


type family ElemF (a :: [k]) (b :: k) :: Bool where
  ElemF a b = Elem b a


class (ElemF a b ~ 'True) => ElemFC (a :: [k]) (b :: k)
instance  (ElemF a b ~ 'True) => ElemFC a b


type family AllSecondInFirst (as :: [k]) (bs :: [k]) = (res :: Bool) where
  AllSecondInFirst as '[] = 'True
  AllSecondInFirst as (b ': bs) = And (Elem b as) (AllSecondInFirst as bs)



type family If (cond :: Bool) (a :: k) (b :: k) where
  If 'True a _ = a
  If 'False _ b = b

type family IfOrErr (cond :: Bool) (a :: k) (err :: ErrorMessage) where
  IfOrErr 'True a _ = a
  IfOrErr 'False _ err = TypeError err

type family TypeEqual (a :: k) (b :: k) :: Bool where
  TypeEqual a a = 'True
  TypeEqual _ _ = 'False


type family Not (a :: Bool) = (res :: Bool)  where
    Not 'False = 'True
    Not 'True = 'False

type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'False a = 'False
    And 'True a = a
    And a 'False = 'False
    And a 'True = a
    And a a = a

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or 'False a = a
    Or 'True a = 'True
    Or a 'False = a
    Or a 'True = 'True
    Or a a = a


type family UniqueElements (a :: [k]) :: Bool where
  UniqueElements '[] = 'True
  UniqueElements (a ': xs) = And (Not (Elem a xs)) (UniqueElements xs)



type family SingleValueFromListOrErr (xs :: [k]) (err :: ErrorMessage) :: k where
  SingleValueFromListOrErr (a ': '[]) _ = a
  SingleValueFromListOrErr xs err = TypeError (err ':$$: 'ShowType xs)


type family UniqueElementsWithErr (a :: [k]) :: Bool where
  UniqueElementsWithErr a = UniqueElementsWithErr' a a

type family UniqueElementsWithErr' (a :: [k]) (b :: [k]) :: Bool where
  UniqueElementsWithErr' '[] _ = 'True
  UniqueElementsWithErr' (a ': xs) b = And (NotWithErr (Elem a xs) (
    'Text "Duplicate element found: " ':<>:
    'ShowType a ':$$:
    'Text " in type list: " ':<>: 'ShowType b
    )) (UniqueElementsWithErr' xs b)


type family NotWithErr (a :: Bool) (err :: ErrorMessage) = (res :: Bool)  where
    NotWithErr 'False _ = 'True
    NotWithErr 'True err = TypeError err



type family TMap (f :: k -> j) (xs :: [k]) :: [j] where
   TMap f '[]       = '[]
   TMap f (x ': xs) = f x ': TMap f xs


type family Union (a :: [k]) (b :: [k]) = (res :: [k]) where
  Union '[] b = b
  Union (a ': xs) b = a ': Union xs b

type family IntercalateSymbol (a :: Symbol) (xs :: [Symbol]) where
  IntercalateSymbol _ '[] = ""
  IntercalateSymbol _ (x ': '[]) = x
  IntercalateSymbol a (x ': xs) = AppendSymbol (AppendSymbol x a) (IntercalateSymbol a xs)

type family Concat (xs :: [[k]]) where
  Concat '[] = '[]
  Concat (a ': xs) = Union a (Concat xs)



type family ExpandApply (fs :: [* -> *]) a = (res :: [*]) where
  ExpandApply ('[]) _ = '[]
  ExpandApply ((b ': rs)) a = b a ': ExpandApply rs a


data Tup a b  = Tup a b


type family Zip (as :: [k]) (bs :: [j]) = (res :: [Tup k j]) where
  Zip '[] '[] = '[]
  Zip (a ': as) (b ': bs) = 'Tup a b ': Zip as bs
  Zip as bs= TypeError ('Text "Type family Zip expected two lists of equal length, got" ':$$: 'ShowType as ':$$: 'ShowType bs)

type family RemoveTypes (remove :: [k]) (xs :: [k]) where
  RemoveTypes _ '[] = '[]
  RemoveTypes remove (x ': xs) = Union (If (Elem x remove) '[] '[x]) (RemoveTypes remove xs)