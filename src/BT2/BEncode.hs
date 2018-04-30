{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BEncode
-- Copyright   :  (c) 2018 Robert Fischer <smokejumperit+stack@gmail.com>
-- License     :  Unlicense
-- Maintainer  :  smokejumperit+stack@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- BEncode (prounounced "Bee-Encode") is a format for the exchange of
-- minimally structured data which is used by BitTorrent.
--
-- It is worth noting that the BEncode format uses the term "string" to
-- mean "an arbitrary sequence of bytes" (which may or may not represent text
-- of some encoding). This module uses the term "bytes" instead of "string"
-- to avoid confusion. Similarly, the BEncode format uses the term
-- "dictionary" to refer to a mapping of these "string" values onto arbitrary
-- other encoded values. We use the term "map", since that is in line with
-- the 'Map' type.
--
-- This module provides support for encoding and decoding from BEncode format
-- in a very high performance way, with special attention paid to reading to
-- and from a 'Handle'.
--
-- Because of the performance emphasis, the specific implementation of the
-- 'BData' type is not exposed. Use the smart constructors and interrogation
-- functions instead. A rich API is provided in the hopes that it covers all
-- the relevant usecases, or at least gets close. If there are missing bits
-- of functionality, please do let the maintainers know, preferably by pull
-- request.
--
-- Another consequence of the emphasis on performance and socket communication
-- is that most of this library is exceedingly eager. Even when we seem to
-- be returning 'LazyText' or 'LazyByteString', you may safely assume that
-- they contents have already been evaluated strictly before being placed
-- into the lazy wrapper. This does mean that we are not doing deferred I/O,
-- and we do need to consume all the bytes in memory before we process them,
-- which is to be construed as a feature.
--
-- Lenses will be provided in a distinct library at some
-- point in the future, since the "lens" package takes for bloody ever to
-- compile and it scares away newbies.
-----------------------------------------------------------------------------

module BT2.BEncode
( -- * Data Type
  BData,
  ToBData(..),
  FromBData(..),
  BMap,
  ToBMap(..),
  FromBMap(..),
  BList,
  ToBList(..),
  FromBList(..),
  BBytes,
  ToBBytes(..),
  FromBBytes(..),
  BInteger,
  ToBInteger(..),
  FromBInteger(..),
  -- * Map Interrogation Functions
  lookupBMap,
  lookupBMapL,
  lookupBMapS,
  lookupBMapT,
  hasKeyBMap,
  hasKeyBMapL,
  hasKeyBMapS,
  hasKeyBMapT,
  -- * Parse Functions
  hGetBData,
  hGetBInteger,
  hGetBList,
  hGetBMap,
  hGetBBytes,
  unpack,
  -- * Export Functions
  hPutBData,
  pack
) where

import           Data.Binary             as Binary
import qualified Data.ByteString         as SBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8   as C8
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as Sbs
import qualified Data.Char               as Char
import qualified Data.Foldable           as F
import qualified Data.List               as List
import           Data.Map.Strict         ( Map )
import qualified Data.Map.Strict         as Map
import           Data.Ratio              ( Ratio, Rational )
import           Data.Sequence           ( Seq, (<|), (|>) )
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           GHC.Exts                ( lazy )
import           Numeric                 ( readDec, readHex, showHex, showInt )
import           RFC.JSON                as JSON
import           RFC.Prelude             hiding ( ByteStringBuilder )
import           RFC.String
import qualified System.IO               as IO
import           Text.Read               ( ReadS, readEither )
import           Text.Show               ( ShowS )

-- | Nifty alias
type ByteStringBuilder = BSB.Builder

-- TODO Add in FromJSON and ToJSON
-- TODO Implement Binary typeclass for BData

-- | Basic datatype for holding BEncode data.
--   Note that everything here is strict and unpacked for maximal memory efficiency, and to better support
--   the over-the-wire communication where they are native. The actual implementation of this type is not
--   a part of the API and is subject to drastic and non-trivial change without warning or major version
--   increment, and is therefore not exposed.
data BData = BBytes {-# UNPACK #-} !BBytes -- ^ Arbitrary bytes, which the spec calls a string.
           | BInteger {-# UNPACK #-} !BInteger -- ^ Arbitrary integer values.
           | BList {-# UNPACK #-} !BList -- ^ List of whatever else we can encode.
           | BMap {-# UNPACK #-} !BMap -- ^ Map of bytes to data, which the spec calls a dictionary.
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | Class for things that can be encoded to a BData.
class ToBData a where
  toBData :: a -> BData

instance {-# OVERLAPPING #-} ToBData BData where
  toBData = id
  {-# INLINE toBData #-}

instance {-# OVERLAPPING #-} ToBData BBytes where
  toBData = BBytes
  {-# INLINE toBData #-}

instance {-# OVERLAPPING #-} ToBData BInteger where
  toBData = BInteger
  {-# INLINE toBData #-}

instance {-# OVERLAPPING #-} ToBData BMap where
  toBData = BMap
  {-# INLINE toBData #-}

class FromBData a where
  fromBData :: MonadFail m => Proxy a -> BData -> m a

failFromBData :: MonadFail m => Proxy a -> BData -> m a
#if MIN_VERSION_base(4,10,0)
failFromBData pxy bad = fail $ "Trying to convert " <> (show pxy) <> " to BData but found " <> (show . typeOf $ bad) <> " => " <> (show bad)
#else
failFromBData pxy bad = fail $ "Trying to convert " <> (show pxy) <> " to BData but found: " <> (show bad)
#endif
{-# INLINE failFromBData #-}

instance {-# OVERLAPPING #-} FromBData BData where
  fromBData _ = return
  {-# INLINE fromBData #-}

instance {-# OVERLAPPING #-} FromBData BMap where -- TODO Do we want to try to coerce lists of pairs to a map?
  fromBData _ (BMap bmap) = return bmap
  fromBData pxy bad       = failFromBData pxy bad
  {-# INLINE fromBData #-}

instance {-# OVERLAPPING #-} FromBData BList where -- TODO Do we want to singleton-ize the rest?
  fromBData _ (BList blist) = return blist
  fromBData pxy bad         = failFromBData pxy bad
  {-# INLINE fromBData #-}

instance {-# OVERLAPPING #-} FromBData BBytes where
  fromBData _ (BBytes bytes) = return bytes
  fromBData pxy bad          = failFromBData pxy bad
  {-# INLINE fromBData #-}

instance {-# OVERLAPPING #-} FromBData BInteger where -- TODO Do we want to try to parse BBytes as UTF-8 string integers?
  fromBData _ (BInteger bint) = return bint
  fromBData pxy bad           = failFromBData pxy bad
  {-# INLINE fromBData #-}

-- | The specific implementations for holding maps
data BMap = BMapEmpty -- ^ For when no values were provided (common case)
          | BMapSingle {-# UNPACK #-} !ShortByteString {-# UNPACK #-} !BData -- ^ For when only one value was provided (common case)
          | BMapMap {-# UNPACK #-} !(Map ShortByteString BData) -- ^ For the general case
  deriving (Show,Eq,Ord,Generic,Typeable)

class ToBMap a where
  toBMap :: a -> BMap

instance {-# OVERLAPPING #-} ToBMap (StrictByteString, BData) where
  toBMap (sbs, bdata) = BMapSingle (Sbs.toShort sbs) bdata
  {-# INLINE toBMap #-}

instance {-# OVERLAPPING #-} ToBMap (Map StrictByteString BData) where
  toBMap rawMap =
    case toList rawMap of
      []     -> BMapEmpty
      [pair] -> toBMap pair
      _      -> BMapMap $ Map.mapKeys Sbs.toShort rawMap
  {-# INLINE toBMap #-}

instance {-# OVERLAPPABLE #-} (Binary k, ToBData v) => ToBMap (Map k v) where
  toBMap = toBMap . Map.map toBData . Map.mapKeys encode
  {-# INLINE toBMap #-}

class FromBMap a where
  fromBMap :: MonadFail m => BMap -> m a

instance {-# OVERLAPS #-} (FromBData v) => FromBMap (StrictByteString, v) where
  fromBMap (sbs, v) = do
    bData <- fromBData v
    BMapSingle (Sbs.toShort sbs) bData
  {-# INLINE toBMap #-}

instance {-# OVERLAPS #-} (FromBData v) => FromBMap (Map StrictByteString v) where
  fromBMap BMapEmpty        = return $ Map.empty
  fromBMap (BMapSingle k v) = do
      let sbs = Sbs.fromShort k
      let vProxy = Proxy :: Proxy v
      vBData <- fromBData vProxy v
      return $ Map.singleton sbs vBData
  fromBMap (BMapMap rawMap) = do
    let keyedPairs = toList $ Map.mapKeys Sbs.fromShort rawMap
    dataPairs <- mapM (\(id,rawData) -> do
        bData <- fromBData (Proxy :: Proxy v) rawData
        return (id,bData)
      ) keyedPairs
    return $ fromList dataPairs
  {-# INLINEABLE fromBMap #-}

instance {-# OVERLAPPABLE #-} (FromBMap k v) => FromBMap [(k,v)] where
  fromBMap = Map.toList . getBMap

-- | The specific implementations for holding lists
data BList = BListEmpty -- ^ For when no values were provided (common case)
           | BListSingle {-# UNPACK #-} !BData -- ^ For when we have only a single value (common case)
           | BListSeq {-# UNPACK #-} !(Seq BData) -- ^ For when we have multiple values
  deriving (Show,Eq,Ord,Generic,Typeable)

class ToBList a where
  toBList :: a -> BList

instance {-# OVERLAPPING #-} ToBData BList where
  toBData = BList
  {-# INLINE toBData #-}

instance {-# OVERLAPPABLE #-} (ToBData a) => ToBList [a] where
  toBList []  = BListEmpty
  toBList [x] = BListSingle $ toBData x
  toBList lst = BListSeq . Seq.fromList $ toBData <$> toList lst
  {-# INLINE toBList #-}

instance {-# OVERLAPPABLE #-} (ToBData a) => ToBList (Seq a) where
  toBList = toBList . toList
  {-# INLINE toBList #-}

class FromBList a where
  fromBList :: (MonadFail m) => Proxy a -> BList -> m a

instance {-# OVERLAPPABLE #-} (FromBList a) => FromBData a where
  fromBData pxy (BList lst) = fromBList pxy lst
  fromBData pxy bad         = failFromBData pxy bad
  {-# INLINE fromBData #-}

instance {-# OVERLAPPABLE #-} (FromBData a) => FromBList [a] where
  fromBList _ BListEmpty        = return []
  fromBList pxy (BListSingle a) = return $ fromBData Proxy a
  fromBList pxy (BListSeq seq)  = mapM (fromBData Proxy) $ toList seq
  {-# INLINE fromBList #-}

-- | The specific implementations for holding bytes.
data BBytes = BBytesEmpty -- ^ For when no bytes were provided (common case)
            | BBytesChar7 {-# UNPACK #-} !Char -- ^ Single ASCII character is often used for keys in messages
            | BBytesText7 {-# UNPACK #-} !StrictText -- ^ Frequent case of value which is ASCII characters
            | BBytesDec7 {-# UNPACK #-} !Word {-# UNPACK #-} !BInteger -- ^ Frequent case of a value which is an ASCII-encoded base-10 string, possibly with some leading zeroes.
            | BBytesHex7 {-# UNPACK #-} !Word {-# UNPACK #-} !BInteger -- ^ Frequent case of value which is an ASCII-encoded hexadecimal string, possibly with some leading zeroes. For this to kick in, we only accept lowercase 'a'..'f' as hex digits.
            | BBytesShort {-# UNPACK #-} !ShortByteString -- ^ For when only a few bytes were provided, although this may also be zero-length.
            | BBytesStrict {-# UNPACK #-} !StrictByteString -- ^ For when lots of bytes were provided, although this may also be zero-length. 'StrictByteString' stores the bytes outside of the heap, which is more efficient for very large blocks.
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | The bytecount below which we prefer 'ShortByteString', and at/above which we prefer 'StrictByteString'.
bbytesLimit :: Int
bbytesLimit = 4096
{-# INLINE bbytesLimit #-}

class ToBBytes a where
  toBBytes :: a -> BBytes

instance {-# OVERLAPPABLE #-} (ToBBytes a) => ToBData a where
  toBData = BBytes . toBBytes
  {-# INLINE toBData #-}

instance {-# OVERLAPPABLE #-} (Binary a) => ToBBytes a where
  toBBytes = toBBytes . LBS.toStrict . Binary.encode
  {-# INLINE toBBytes #-}

class FromBBytes a where
  fromBBytes :: Proxy a -> BBytes -> a

instance {-# OVERLAPPING #-} FromBBytes StrictByteString where
  fromBBytes _ BBytesEmpty = SBS.empty
  fromBBytes _ (BBytesShort short) = Sbs.fromShort short
  fromBBytes _ (BBytesStrict strict) = strict
  fromBBytes _ (BBytesChar7 c) = C8.singleton c
  fromBBytes _ (BBytesText7 txt) = T.encodeUtf8 txt
  fromBBytes _ (BBytesDec7 zeroes n) =
    T.encodeUtf8 . T.pack $
    List.replicate (fromIntegral zeroes) '0' <> showInt (getBInteger n) ""
  fromBBytes _ (BBytesHex7 zeroes n) =
    T.encodeUtf8 . T.pack $
    List.replicate (fromIntegral zeroes) '0' <> showHex (getBInteger n) ""
  {-# INLINEABLE fromBBytes #-}

-- | The specific implementation of holding integer values
data BInteger = BIntegerWord8 {-# UNPACK #-} !Word8 -- ^ When we have a Word8, which is pretty common (port numbers, etc.)
              | BIntegerWord {-# UNPACK #-} !Word -- ^ When we have a machine-sized word, which is pretty much all the cases
              | BIntegerWordNeg {-# UNPACK #-} !Word -- ^ When we have a negative-valued int whose magnitude fits as a machine-sized word
              | BIntegerZ {-# UNPACK #-} !Integer -- ^ Fallback implementation for all other cases
              | BIntegerZero -- ^ Specific case for 0 (used like 'false')
              | BIntegerOne -- ^ Specific case for 1 (used like 'true')
              | BIntegerTwo -- ^ Specific case for 2 (used for the current version number of the BitTorrent protocol)
  deriving (Show,Eq,Ord,Generic,Typeable)

class ToBInteger a where
  toBInteger :: a -> BInteger

instance {-# OVERLAPPABLE #-} (ToBInteger a) => ToBData a where
  toBData = BInteger . toBInteger

class FromBInteger a where
  fromBInteger :: (MonadFail m) => BInteger -> m a

instance {-# OVERLAPPABLE #-} (FromBInteger a) => FromBData a where
  fromBData (BInteger z) = fromBInteger z

instance {-# OVERLAPPING #-} ToBBytes StrictByteString where
  toBBytes sbs
    | SBS.null sbs = BBytesEmpty
    | SBS.length sbs <= bbytesLimit = BBytesShort $ Sbs.toShort sbs
    | otherwise = BBytesStrict sbs
  {-# INLINE toBBytes #-}

instance {-# OVERLAPPING #-} ToBBytes LazyByteString where
  toBBytes lbs =
    if LBS.null bs then
      BBytesEmpty
    else
      toBBytes $! LBS.toStrict bs
  {-# INLINE toBBytes #-}

instance {-# OVERLAPPING #-} ToBBytes Char where
  toBBytes '0' = BBytesDec7 0 BIntegerZero
  toBBytes '1' = BBytesDec7 0 BIntegerOne
  toBBytes '2' = BBytesDec7 0 BIntegerTwo
  toBBytes '3' = BBytesDec7 0 $ BIntegerWord8 3
  toBBytes '4' = BBytesDec7 0 $ BIntegerWord8 4
  toBBytes '5' = BBytesDec7 0 $ BIntegerWord8 5
  toBBytes '6' = BBytesDec7 0 $ BIntegerWord8 6
  toBBytes '7' = BBytesDec7 0 $ BIntegerWord8 7
  toBBytes '8' = BBytesDec7 0 $ BIntegerWord8 8
  toBBytes '9' = BBytesDec7 0 $ BIntegerWord8 9
  toBBytes 'a' = BBytesHex7 0 $ BIntegerWord8 10
  toBBytes 'b' = BBytesHex7 0 $ BIntegerWord8 11
  toBBytes 'c' = BBytesHex7 0 $ BIntegerWord8 12
  toBBytes 'd' = BBytesHex7 0 $ BIntegerWord8 13
  toBBytes 'e' = BBytesHex7 0 $ BIntegerWord8 14
  toBBytes 'f' = BBytesHex7 0 $ BIntegerWord8 15
  toBBytes c
    | Char.isAscii c = BBytesChar7 c
    | otherwise = toBBytes $! asUTF8 [c]
{-# INLINE mkBChar #-}

instance {-# OVERLAPPING #-} ToBBytes String where
  toBBytes []  = BBytesEmpty
  toBBytes [c] = toBBytes c
  toBBytes str
      | leadingZeroCnt > 1 && null afterZeros = BBytesDec7 (leadingZeroCnt - 1) BIntegerZero -- All zeroes: not handled properly below
      | List.all Char.isDigit str = BBytesDec7 leadingZeroCnt $! readToIntegral readDec
      | List.all isHex str = BBytesHex7 leadingZeroCnt $! readToIntegral readHex
      | List.all Char.isAscii str = BBytesText7 $! T.pack str
      | otherwise = toBBytes $! asUTF8 str
    where
      isHex c = Char.isHexDigit c && (Char.isDigit c || Char.isLower c) -- Only accept lowercase encodings.
      readToIntegral parser =
        case parser afterZeros of
          (x,_):_ -> mkBInteger x
          _       -> BIntegerZero
      leadingZeroes :: String
      afterZeros :: String
      (leadingZeroes, afterZeros) = List.span ('0' ==) str
      leadingZeroCnt = fromIntegral . List.length $ leadingZeroes :: Word
  {-# INLINEABLE mkBString #-}

instance {-# OVERLAPPABLE #-} (ToText a) => ToBBytes a where
  toBBytes txtSrc
      | T.all Char.isAscii txt = BBytesText7 txt
      | otherwise = toBBytes . fromText $! txt
    where
      txt = toText txtSrc
  {-# INLINE toBBytes #-}
  {-# SPECIALIZE instance ToBBytes LazyText #-}
  {-# SPECIALIZE instance ToBBytes StrictText #-}

instance {-# OVERLAPPABLE #-} (Integral z) => ToBInteger z where
  toBInteger = toBInteger . toInteger
  {-# INLINE toBInteger #-}

instance {-# OVERLAPPING #-} ToBInteger Integer where
  toBInteger z =
    case z of
      0 -> BIntegerZero
      1 -> BIntegerOne
      2 -> BIntegerTwo
      i
        | i < 0 && (-1 * i) <= maxWord  -> BIntegerWordNeg $! fromInteger (-1 * i)
        | i < 0  -> BIntegerZ $! i
        | i <= maxWord8 -> BIntegerWord8 $! fromInteger i
        | i <= maxWord -> BIntegerWord $! fromInteger i
        | otherwise -> BIntegerZ $! i
    where
      maxWord8 = toInteger (maxBound :: Word8)
      maxWord = toInteger (maxBound :: Word)
  {-# INLINE toBInteger #-}

instance {-# OVERLAPPABLE #-} (Foldable f) => ToBList f where
  toBList datas =
    case F.toList $! datas of
      []   -> BListEmpty
      [x]  -> BListSingle x
      rest -> BListSeq (Seq.fromList rest)
  {-# INLINE toBList #-}

instance {-# OVERLAPPABLE #-} (ToBData a) => ToBList (Seq a) where
  toBList datas =
    case toList datas of
      []  -> BListEmpty
      [x] -> BListSingle $ toBData x
      _   -> BListSeq . toBData <$> datas
  {-# INLINE toBList #-}

instance {-# OVERLAPPABLE #-} (ToBBytes k, ToBData v) => ToBMap (Map k v) where
  toBMap = toBMap . Map.map toBData . Map.mapKeys toBBytes
  {-# INLINE toBMap #-}

instance {-# OVERLAPPING #-} FromBBytes LazyByteString where
  fromBBytes = fromBBytes . LBS.fromStrict
  {-# INLINE fromBBytes #-}

instance {-# OVERLAPPING #-} FromBBytes StrictText where
  fromBBytes :: (MonadFail m) => Proxy StrictText -> BBytes -> m StrictText
  fromBBytes = do
    bytes <- fromBBytes
    case decodeText (UTF8 bytes) of
      Nothing  -> fail $ "Could not decode text from bytes => " <> (show bytes)
      Just txt -> return txt
  {-# INLINE fromBBytes #-}

instance {-# OVERLAPPING #-} FromBBytes String where
  fromBBytes = C8.unpack <$> fromBBytes
  {-# INLINE fromBBytes #-}

instance {-# OVERLAPPABLE #-} (FromText txt) => FromBBytes txt where
  fromBBytes = fromText <$> fromBBytes
  {-# INLINE fromBBytes #-}
  {-# SPECIALIZE instance FromBBytes StrictText #-}
  {-# SPECIALIZE instance FromBBytes LazyText   #-}

instance {-# OVERLAPPABLE #-} (IsList lst, Item lst ~ item, FromBData item) => FromBList lst where
  fromBList _ blist = fromList . mapM fromBData <$>
    case blist of
      BListEmpty      -> []
      (BListSingle x) -> [x]
      (BListSeq seq)  -> toList seq
  {-# INLINE fromBList #-}

instance {-# OVERLAPPING #-} FromBInteger Integer  where
  fromBInteger i =
    case i of
      (BIntegerWord w)    -> toInteger w
      (BIntegerWord8 w)   -> toInteger w
      (BIntegerWordNeg w) -> -1 * (toInteger w)
      (BIntegerZ z)       -> z
      BIntegerZero        -> 0
      BIntegerOne         -> 1
      BIntegerTwo         -> 2
  {-# INLINE fromBInteger #-}

instance {-# OVERLAPPABLE #-} (Num n) => FromBInteger n where
  fromBInteger = fromInteger . getBInteger
  {-# INLINE fromBInteger #-}
  {-# SPECIALIZE instance FromBInteger Int      #-}
  {-# SPECIALIZE instance FromBInteger Word     #-}
  {-# SPECIALIZE instance FromBInteger Word8    #-}
  {-# SPECIALIZE instance FromBInteger Double   #-}
  {-# SPECIALIZE instance FromBInteger Float    #-}
  {-# SPECIALIZE instance FromBInteger Rational #-}


instance {-# OVERLAPS #-} (FromBData v) => FromBMap (Map LazyByteString v) where
  fromBMap = Map.mapKeys LBS.fromStrict . fromBMap
  {-# INLINE fromBMap #-}

instance {-# OVERLAPS #-} (FromBData v) => FromBMap (Map String v) where
  fromBMap bmap =
    let pairs = Map.toList $ getBMap bmap in
    let strKeyPairs = (\(k,v) -> (\s -> (s,v)) <$> toUTF8 k) <$> pairs in
    Map.fromList $ catMaybes strKeyPairs
  {-# INLINE fromBMap #-}

instance {-# OVERLAPPABLE #-} (FromText str, Ord str, FromBData v) => FromBMap (Map str v) where
  fromBMap = convertKeys . getBMapS
    where
      convertKeys = Map.mapKeys toTextish
      toTextish = fromText . toText
  {-# INLINE fromBMap #-}
  {-# SPECIALIZE instance (FromBData v) => FromBMap LazyText v   #-}
  {-# SPECIALIZE instance (FromBData v) => FromBMap StrictText v #-}


-- | Similar to 'hLookAhead', but providing access to the next char instead of the next byte.
hLookAheadChar :: (MonadIO m) => Handle -> m Char
hLookAheadChar = liftIO . IO.hLookAhead
{-# INLINE hLookAheadChar #-}
{-# SPECIALIZE INLINE hLookAheadChar :: Handle -> IO Char #-}

-- | Skips a single character from the handle
hSkipChar :: (MonadIO m) => Handle -> m ()
hSkipChar = hIfEOF (return ()) (getChar)
  where
    getChar handle = void . liftIO $ IO.hGetChar handle

-- | Protects against the case of being at the end of the file.
hIfEOF :: (MonadIO m) => m a -> (Handle -> m a) -> Handle -> m a
hIfEOF defaultValue f handle = do
  handleClosed <- hIsClosed handle
  handleReadable <- hIsReadable handle
  eofReached <- hIsEOF handle
  if handleClosed || (not handleReadable) || eofReached then
    defaultValue
  else
    f handle
{-# INLINEABLE hIfEOF #-}
{-# SPECIALIZE hIfEOF :: IO a -> (Handle -> IO a) -> Handle -> IO a #-}

-- | Reads a 'BData' of some arbitrary kind from the handle.
--   Fails if the handle is at EOF.
hGetBData :: (MonadIO m, MonadFail m, FromBData bdata) => Handle -> m bdata
hGetBData = hIfEOF (fail "Handle is at end of file or otherwise unreadable") $ \handle -> do
  nextChar <- hLookAheadChar handle
  case nextChar of
    'i' -> (hGetBInteger handle :: m BInteger) >>= fromBData
    'l' -> (hGetBList handle :: m BList) >>= fromBData
    'd' -> (hGetBMap handle :: m BMap) >>= fromBData
    c   | Char.isDigit c -> (hGetBBytes handle :: m BBytes) >>= fromBData
    _   -> fail $ "Badly formed BEncoded data: " <> [nextChar]
{-# INLINEABLE hGetBData #-}
{-# SPECIALIZE hGetBData :: Handle -> IO BData #-}

-- | Reads an integer, failing if it is EOF or not a properly BEncoded integer.
hGetBInteger :: (MonadIO m, MonadFail m, FromBInteger binteger) => Handle -> m binteger
hGetBInteger = hIfEOF (fail "At EOF when we expected an integer") $ \handle -> do
    signalChar <- liftIO $ IO.hGetChar handle
    case signalChar of
      'i' -> getDigits handle >>= fromBInteger
      _   -> fail $ "Expected the integer start character (i), but got: " <> [signalChar]
  where
    getDigits handle = do
      digitsLbs <- BSB.toLazyByteString <$> getDigits' mempty handle
      digitsStr <- toUTF8 digitsLbs
      case readEither digitsStr of
        Left msg  -> fail $ "Could not read the digits string: " <> digitsStr <> " => " <> msg
        Right (num::Integer) -> return $ toBInteger num
    getDigits' memo handle = do
      nextChar <- hLookAheadChar handle
      case nextChar of
        '-' ->
          hSkipChar handle >> getDigits' (BSB.char7 '-' <> memo) handle
        'e' ->
          hSkipChar handle >> (return $! memo)
        c | Char.isDigit c ->
          hSkipChar handle >> getDigits' (memo <> BSB.char7 c) handle
        _ ->
          fail $ "Expected digit or integer end character (e), but got: " <> [nextChar]
{-# INLINEABLE hGetBInteger #-}
{-# SPECIALIZE hGetBInteger :: Handle -> IO BInteger #-}

-- | Reads a list, returning the empty list if it is EOF and failing if it is not a properly BEncoded list.
hGetBList :: (MonadIO m, MonadFail m) => Handle -> m BList
hGetBList = hIfEOF (return BListEmpty) $ \handle -> do
    signalChar <- hLookAheadChar handle
    case signalChar of
      'l' -> hSkipChar handle >> mkBSeq <$> getElements handle
      _   -> fail $ "Expected the list start character (i), but got: " <> [signalChar]
  where
    getElements = getElements' mempty
    getElements' memo handle = do
      nextChar <- hIfEOF (return 'e') (hLookAheadChar) handle
      case nextChar of
        'e' ->
          hSkipChar handle >> (return $! memo)
        _ -> do
          nextData <- hGetBData handle
          getElements' (memo |> nextData) handle
{-# INLINEABLE hGetBList #-}
{-# SPECIALIZE hGetBList :: Handle -> IO BList #-}

-- | Gets a map/dictionary from the handle, returning the empty map if it is at EOF and failing if it is not a properly BEncoded dictionary.
--   Note that this implementation is tolerant if the keys are out of order.
hGetBMap :: (MonadIO m, MonadFail m) => Handle -> m BMap
hGetBMap = hIfEOF (return BMapEmpty) $ \handle -> do
    signalChar <- liftIO $ IO.hGetChar handle
    case signalChar of
      'd' -> liftIO $ mkBMap <$> getContents handle
      _   -> fail $ "Expected the dictionary start character (d), but got: " <> [signalChar]
  where
    getContents :: Handle -> IO (Map StrictByteString BData)
    getContents = getContents' Map.empty
    getContents' :: Map StrictByteString BData -> Handle -> IO (Map StrictByteString BData)
    getContents' memo = hIfEOF (return $! memo) $ \handle -> do
      nextChar <- hLookAheadChar handle
      case nextChar of
        'e' ->
          return $! memo
        _ -> do
          keyBytes <- getBBytes <$> hGetBBytes handle
          value <- hGetBData handle
          getContents' (Map.insert keyBytes value memo) handle
{-# INLINEABLE hGetBMap #-}
{-# SPECIALIZE hGetBMap :: Handle -> IO BMap #-}

-- | Gets the bytes of what the spec calls a "string" from the handle, returning a zero-length bytestring if it is at EOF and
--   failing if it is not a properly BEncoded string.
hGetBBytes :: (MonadIO m) => Handle -> m BBytes
hGetBBytes = getBytes
  where
    getBytes =
      hIfEOF (return BBytesEmpty) (\handle -> liftIO $ do
        byteCount <- getByteCount handle
        if byteCount == 0 then
          return BBytesEmpty
        else
          do
            bytes <- getBytes' byteCount mempty handle
            let sbs = LBS.toStrict . BSB.toLazyByteString $ bytes
            return $!
              if SBS.null sbs then
                BBytesEmpty
              else if SBS.length sbs <= bbytesLimit then
                BBytesShort $! Sbs.toShort sbs
              else
                BBytesStrict sbs
      )
    getBytes' cnt memo =
      if cnt <= 0 then
        const . return $! memo
      else
        hIfEOF (return $! memo) $ \handle -> do
          bs <- C8.hGet handle cnt
          getBytes' (cnt - olength bs) (memo <> BSB.byteString bs) handle
    getByteCount = hIfEOF (return 0) (fmap read . getDigits)
    getDigits = hIfEOF (return "0") (fmap toList . getDigits' mempty)
    getDigits' memo = hIfEOF (return memo) $ \handle -> do
      nextChar <- hLookAheadChar handle
      case nextChar of
        ':' ->
          hSkipChar handle >> return memo
        c | Char.isDigit c ->
          hSkipChar handle >> getDigits' (memo |> c) handle
        _ ->
          return memo
{-# INLINEABLE hGetBBytes #-}
{-# SPECIALIZE hGetBBytes ::  Handle -> IO BBytes #-}

-- | Looks up a value from a 'BData' assuming that it is a map.
lookupBMap :: (MonadFail m) => StrictByteString -> BMap -> m BData
lookupBMap key bdata =
  let bmap = fromBData bdata in
  let maybeValue = Map.lookup key bmap in
  maybe (fail $ "Could not find key: " <> show key) (return) maybeValue
{-# INLINE lookupBMap #-}
{-# SPECIALIZE INLINE lookupBMap :: StrictByteString -> BMap -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMap :: StrictByteString -> BMap -> Maybe BData #-}

-- | Looks up a value from a 'BData' assuming that it is a map.
lookupBMapL :: (MonadFail m) => LazyByteString -> BMap -> m BData
lookupBMapL = lookupBMap . LBS.toStrict
{-# INLINE lookupBMapL #-}
{-# SPECIALIZE INLINE lookupBMapL :: LazyByteString -> BMap -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapL :: LazyByteString -> BMap -> Maybe BData #-}

-- | Looks up a value from a 'BData' assuming it is a map whose keys are encoded in UTF8.
--   The key must be a string proper: if you have something that's string-like, use 'lookupBMapT'.
lookupBMapS :: (MonadFail m) => String -> BMap -> m BData
lookupBMapS = lookupBMap . asUTF8
{-# INLINE lookupBMapS #-}
{-# SPECIALIZE INLINE lookupBMapS :: String -> BMap -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapS :: String -> BMap -> Maybe BData #-}

-- | Looks up a value from a 'BMap' assuming that it is a map whose keys are encoded in UTF8.
--   The key can be of any type that supports 'ToText'.
lookupBMapT :: (MonadFail m, ToText a) => a -> BMap -> m BData
lookupBMapT = lookupBMap . asUTF8
{-# INLINE lookupBMapT #-}
{-# SPECIALIZE INLINE lookupBMapT :: LazyText -> BMap -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: LazyText -> BMap -> Maybe BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: StrictText -> BMap -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: StrictText -> BMap -> Maybe BData #-}

-- | Checks to see if the 'BMap' is a map containing the given key.
hasKeyBMap :: StrictByteString -> BMap -> Bool
hasKeyBMap key = Map.member key . getBMap
{-# INLINE hasKeyBMap #-}

-- | Checks to see if the 'BMap' is a map containing the given key.
hasKeyBMapL :: LazyByteString -> BMap -> Bool
hasKeyBMapL = hasKeyBMap . LBS.toStrict
{-# INLINE hasKeyBMapL #-}

-- | Checks to see if the 'BMap' is a map containing the given key encoded in UTF8.
hasKeyBMapS :: String -> BMap -> Bool
hasKeyBMapS = hasKeyBMap . asUTF8
{-# INLINE hasKeyBMapS #-}

-- | Checks to see if the 'BMap' is a map containing the given key encoded in UTF8.
--   The key can be of any type that supports 'ToText'.
hasKeyBMapT :: (ToText a) => a -> BMap -> Bool
hasKeyBMapT = hasKeyBMap . asUTF8
{-# INLINE hasKeyBMapT #-}
{-# SPECIALIZE INLINE hasKeyBMapT :: LazyText -> BMap -> Bool #-}
{-# SPECIALIZE INLINE hasKeyBMapT :: StrictText -> BMap -> Bool #-}

instance {-# OVERLAPPABLE #-} (Foldable lst, Binary k, ToBData v) => ToBMap (lst (k, v)) where
  toBMap = toBMap . Map.fromList . map (\(k,v) -> (Sbs.toShort . LBS.toStrict $ encode k, toBData v)) . F.toList
  {-# INLINE toBMap #-}
  {-# SPECIALIZE instance (ToBData v) => ToBMap [(StrictByteString, v)] #-}
  {-# SPECIALIZE instance (ToBData v) => ToBMap [(LazyByteString, v)]   #-}

-- | Given a 'Handle' and a 'BData', write the binary representation of the 'BData' to the 'Handle'.
--   The 'Handle' should be in 'BlockBuffering' mode, because we're going to dump a big hunk of
--   binary onto it.
hPutBData :: (MonadIO m) => Handle -> BData -> m ()
hPutBData handle = liftIO . BSB.hPutBuilder handle . unpack
{-# INLINE hPutBData #-}
{-# SPECIALIZE INLINE hPutBData :: Handle -> BData -> IO () #-}

-- | Unpacks the 'BData' into a 'BSB.Builder'. You can go from there to your favorite kind of 'ByteString',
--   or even directly into a 'Handle' via 'BSB.hPutBuilder'.
unpack :: BData -> BSB.Builder
unpack = lazy . build . lazy
  where
    build (BBytes BBytesEmpty) = BSB.string7 "0:"
    build (BBytes (BBytesDec7 zeroCnt bint)) =
      let decBuilder = BSB.integerDec . getBInteger $ bint in
      let zeroesBuilder = BSB.string7 $ List.replicate (fromIntegral zeroCnt) '0' in
      let lbs = BSB.toLazyByteString $ zeroesBuilder <> decBuilder in
      BSB.intDec (fromIntegral $ LBS.length lbs) <> BSB.char7 ':' <> BSB.lazyByteString lbs
    build (BBytes (BBytesHex7 zeroCnt bint)) =
      let hexBuilder = BSB.string7 . toHex . getBInteger $ bint in
      let zeroesBuilder = BSB.string7 $ List.replicate (fromIntegral zeroCnt) '0' in
      let lbs = BSB.toLazyByteString $ zeroesBuilder <> hexBuilder in
      BSB.intDec (fromIntegral $ LBS.length lbs) <> BSB.char7 ':' <> BSB.lazyByteString lbs
    build (BBytes (BBytesChar7 c)) = BSB.string7 "1:" <> BSB.char7 c
    build (BBytes (BBytesText7 txt)) = BSB.intDec (T.length txt) <> BSB.char7 ':' <> (BSB.string7 . T.unpack) txt
    build (BBytes (BBytesShort sbs)) = BSB.intDec (Sbs.length sbs) <> BSB.char7 ':' <> BSB.shortByteString sbs
    build (BBytes (BBytesStrict sbs)) = BSB.intDec (SBS.length sbs) <> BSB.char7 ':' <> BSB.byteString sbs
    build (BInteger BIntegerZero) = BSB.string7 "i0e"
    build (BInteger BIntegerOne) = BSB.string7 "i1e"
    build (BInteger BIntegerTwo) = BSB.string7 "i2e"
    build (BInteger (BIntegerZ z)) = BSB.char7 'i' <> BSB.integerDec z <> BSB.char7 'e'
    build (BInteger (BIntegerWordNeg w)) = BSB.string7 "i-" <> BSB.wordDec w <> BSB.char7 'e'
    build (BInteger (BIntegerWord w)) = BSB.char7 'i' <> BSB.wordDec w <> BSB.char7 'e'
    build (BInteger (BIntegerWord8 w)) = BSB.char7 'i' <> BSB.word8Dec w <> BSB.char7 'e'
    build (BList BListEmpty) = BSB.string7 "le"
    build (BList (BListSingle bdata)) = BSB.char7 'l' <> (build bdata) <> BSB.char7 'e'
    build (BList (BListSeq bdatas)) = lazy $ BSB.char7 'l' <> (foldr (\cur rest -> build cur <> rest) mempty bdatas) <> BSB.char7 'e'
    build (BMap BMapEmpty) = BSB.string7 "de"
    build (BMap (BMapSingle sbs bdata)) = BSB.char7 'd' <> (buildKey sbs) <> (build bdata) <> BSB.char7 'e'
    build (BMap (BMapMap bmap)) = lazy $ -- Ordering must be "binary ascending key", and that's guaranteed by 'Map.foldrWithkey'
      BSB.char7 'd'
      <> (Map.foldrWithKey (\key val rest -> buildKey key <> build val <> rest) mempty bmap)
      <> BSB.char7 'e'
    buildKey = build . BBytes . BBytesShort
    toHex z = showHex z ""
{-# INLINEABLE unpack #-}


