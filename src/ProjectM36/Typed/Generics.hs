module ProjectM36.Typed.Generics where


import RIO
import qualified RIO.Text as T
import Generics.SOP
import Generics.SOP.NP

import Generics.SOP.Dict

import Data.Kind (Type)
import GHC.TypeLits

--import Type.Reflection
-- import Data.Type.Equality

showSymbol :: (KnownSymbol a) => proxy a -> T.Text
showSymbol p = T.pack $ symbolVal p

showSymbols :: forall proxy xs. (All KnownSymbol xs) => proxy xs -> [T.Text]
showSymbols _ = cfoldMap_NP (Proxy :: Proxy KnownSymbol) (pure . showSymbol) (pure_NP Proxy :: NP Proxy xs)

{-
-- leaving this here as an example of how to provide kind matching across the function - in the version above the NP (Dict c) effectively does this for us)
catMapOverNPDict1 ::(Monad m, SListI (xs :: [k])) =>  NP (Dict Typeable) xs -> (forall proxy (a :: k). Typeable a => proxy a -> m [b]) -> m [b]
catMapOverNPDict1 sc f= fmap concat . sequence $ applyOverNPDict sc f-}


catMapOverNPDict ::(Monad m, SListI xs, c ~ Typeable) =>  NP (Dict c) xs -> (forall proxy a. c a => proxy a -> m [b]) -> m [b]
catMapOverNPDict sc f= fmap concat . sequence $ applyOverNPDict sc f

applyOverNPDict :: (SListI xs) => NP (Dict c) xs -> (forall proxy a. c a => proxy a -> z) -> [z]
applyOverNPDict np f = collapse_NP $ map_NP (\(d :: Dict c b) -> K $ (withDict d f) d) np



toNPDict :: (All c xs) => Proxy xs -> NP (Dict c) xs
toNPDict _ = unAll_NP Dict


class WithCDictionary c a where
  type CDictionaryList c a :: [Type]
  toCDictionary :: a -> NP (Dict c) (CDictionaryList c a)
  withCDictionary :: (SListI (CDictionaryList c a)) => (NP (Dict c) (CDictionaryList c a) -> r) -> a -> r






{-
POC of getting names
getFieldName :: forall a xs (b :: *). (HasDatatypeInfo a, All Typeable xs,  IsProductType a xs, Typeable b) => (a -> b) -> [Text]
getFieldName _ =
  case datatypeInfo (Proxy :: Proxy a) of
    ADT _ _ cInfoNp -> catMaybes . concat $ collapse_POP $ reduceWithEquality $ toFieldNamesPop cInfoNp

    _ -> error "getFieldName"
  where

    toFieldNamesPop :: (SListI zs) => NP ConstructorInfo zs -> POP (FieldInfo) zs
    toFieldNamesPop a = POP $ map_NP toFieldNames a

    toFieldNames :: ConstructorInfo zs -> NP FieldInfo zs
    toFieldNames (Constructor _) = error "toFieldNames"
    toFieldNames (Infix _ _ _) = error "toFieldNames"
    toFieldNames (Record _ fis) =fis


    reduceWithEquality :: (All2 Typeable xs1) => POP FieldInfo xs1 -> POP (K (Maybe Text)) xs1
    reduceWithEquality np = cmap_POP (Proxy :: Proxy Typeable) getEqualName np
    getEqualName :: forall a1. Typeable a1 => (FieldInfo a1) -> K (Maybe Text) a1
    getEqualName (FieldInfo n) =
      case testEquality (typeRep :: TypeRep a1) (typeRep :: TypeRep b) of
        Nothing -> K Nothing
        (Just _) -> K (Just (T.pack n))
-}