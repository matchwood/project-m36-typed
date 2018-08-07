module ProjectM36.Typed.Types where
import RIO
import qualified Data.UUID as UUID
import Data.Binary
import qualified RIO.ByteString as B
import qualified RIO.List.Partial as LPartial
import qualified Data.Word8 as W8

import qualified System.Random as R
import Test.QuickCheck.Arbitrary
-- @todo remove this as we don't actually want these instances in the long run
import Test.QuickCheck.Instances()

newtype ETag = ETag UUID.UUID
  deriving (Eq, Ord, Show, Generic, Arbitrary)
  deriving newtype (NFData, Binary)

-- @todo replace UUID with something else more cryptographically secure?

randomEtag :: (R.RandomGen g) => g -> (ETag, g)
randomEtag g =
  let (uid, nextG) = R.random g
  in (ETag uid, nextG)


instance R.Random ETag where
  randomR _ = R.random
  random = randomEtag


newtype SafeId = SafeId B.ByteString
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Binary)



safeIdToByteString :: SafeId -> B.ByteString
safeIdToByteString (SafeId bs) = bs

safeIdFromByteString :: B.ByteString -> Either Text SafeId
safeIdFromByteString bs
  | B.length bs /= safeIdLength = Left . utf8BuilderToText $ "Could not parse SafeId from " <> displayShow bs <> ": length of ByteString should be exactly " <> displayShow safeIdLength
  | isBadWord bs = Left . utf8BuilderToText $ "Could not parse SafeId from " <> displayShow bs <> ": the ByteString matches with a known bad word"
  | otherwise = Right. SafeId $ bs

safeIdLength :: Int
safeIdLength = 10

randomSafeId :: (R.RandomGen g) => g -> (SafeId, g)
randomSafeId gen =
  let (nextGen, ws) = (generateW8s safeIdLength gen)
      idStr = B.pack ws
  in
    case isBadWord idStr of
      True -> randomSafeId (nextGen)
      False -> (SafeId idStr, nextGen)


instance R.Random SafeId where
  randomR _ = R.random
  random = randomSafeId

generateW8s :: forall g. (R.RandomGen g) => Int -> g -> (g, [W8.Word8])
generateW8s num gen = foldr addW8 (gen, []) [1 .. num]
  where
    addW8 :: Int -> (g, [W8.Word8]) ->  (g, [W8.Word8])
    addW8 _ (thisGen, ws) =
      let (idx, newGen) = R.randomR (0, (base58PoolLength - 1)) thisGen
      in (newGen, (base58Pool LPartial.!! idx) : ws)


byteStringToReadable :: B.ByteString -> B.ByteString
byteStringToReadable bs = B.map (W8.toLower . numericReplacements) bs

base58PoolLength :: Int
base58PoolLength = length base58Pool

base58Pool :: [W8.Word8]
base58Pool = [W8._1, W8._2, W8._3, W8._4, W8._5, W8._6, W8._7, W8._8, W8._9, W8._a, W8._b, W8._c, W8._d, W8._e, W8._f, W8._g, W8._h, W8._i, W8._j, W8._k, W8._m, W8._n, W8._o, W8._p, W8._q, W8._r, W8._s, W8._t, W8._u, W8._v, W8._w, W8._x, W8._y, W8._z, W8._A, W8._B, W8._C, W8._D, W8._E, W8._F, W8._G, W8._H, W8._J, W8._K, W8._L, W8._M, W8._N, W8._P, W8._Q, W8._R, W8._S, W8._T, W8._U, W8._V, W8._W, W8._X, W8._Y, W8._Z ]

numericReplacements :: W8.Word8 -> W8.Word8
numericReplacements w
  | w == W8._1 = W8._l
  | w == W8._2 = W8._r
  | w == W8._3 = W8._e
  | w == W8._4 = W8._a
  | w == W8._5 = W8._s
  | w == W8._6 = W8._b
  | w == W8._7 = W8._t
  | w == W8._8 = W8._b
  | w == W8._9 = W8._g
  | otherwise = w

isBadWord :: B.ByteString -> Bool
isBadWord bs = or $ map (\w -> B.isInfixOf w (byteStringToReadable bs)) badWords

badWords :: [B.ByteString]
badWords = []