{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- Lenses will be provided in a distinct library at some
-- point in the future, since the "lens" package takes for bloody ever to
-- compile and it scares away newbies.
-----------------------------------------------------------------------------

module BEncode
( -- * Data Type
  BData,
  -- * Smart constructors
  mkBBytes,
  mkBBytesL,
  mkBChar,
  mkBString,
  mkBText,
  mkBInteger,
  mkBList,
  mkBListO,
  mkBSeq,
  mkBMap,
  mkBMapL,
  mkBMapS,
  mkBMapT,
  -- * General Interrogation Functions
  getBBytes,
  getBBytesL,
  getBString,
  getBText,
  getBInteger,
  getBNum,
  getBList,
  getBMap,
  getBMapL,
  getBMapS,
  getBMapT,
  -- * Map Interrogation Functions
  lookupBMap,
  lookupBMapL,
  lookupBMapS,
  lookupBMapT,
  hasKeyBMap,
  hasKeyBMapL,
  hasKeyBMapS,
  hasKeyBMapT,
  -- * Map to List conversions
  fromPairsBMap,
  fromPairsBMapL,
  fromPairsBMapO,
  fromPairsBMapS,
  fromPairsBMapT,
  toPairsBMap,
  toPairsBMapL,
  toPairsBMapS,
  toPairsBMapT,
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
import           GHC.Exts                ( lazy )
import           RFC.JSON                as JSON
import           RFC.Prelude             hiding ( ByteStringBuilder )
import           RFC.String
import qualified System.IO               as IO
import           Text.Read               ( readEither )

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

-- | The specific implementations for holding maps
data BMap = BMapEmpty -- ^ For when no values were provided (common case)
          | BMapSingle {-# UNPACK #-} !ShortByteString {-# UNPACK #-} !BData -- ^ For when only one value was provided (common case)
          | BMapMap {-# UNPACK #-} !(Map ShortByteString BData) -- ^ For the general case
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | The specific implementations for holding lists
data BList = BListEmpty -- ^ For when no values were provided (common case)
           | BListSingle {-# UNPACK #-} !BData -- ^ For when we have only a single value (common case)
           | BListSeq {-# UNPACK #-} !(Seq BData) -- ^ For when we have multiple values
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | The specific implementations for holding bytes.
data BBytes = BBytesEmpty -- ^ For when no bytes were provided (common case)
            | BBytesChar7 {-# UNPACK #-} !Char -- ^ Single ASCII character is often used for keys in messages
            | BBytesShort {-# UNPACK #-} !ShortByteString -- ^ For when only a few bytes were provided, although this may also be zero-length.
            | BBytesStrict {-# UNPACK #-} !StrictByteString -- ^ For when lots of bytes were provided, although this may also be zero-length. 'StrictByteString' stores the bytes outside of the heap, which is more efficient for very large blocks.
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | The bytecount below which we prefer 'ShortByteString', and at/above which we prefer 'StrictByteString'.
bbytesLimit :: Int
bbytesLimit = 4096

-- | The specific implementation of holding integer values
data BInteger = BIntegerWord8 {-# UNPACK #-} !Word8 -- ^ When we have a Word8, which is pretty common (port numbers, etc.)
              | BIntegerWord {-# UNPACK #-} !Word -- ^ When we have a machine-sized word, which is pretty much all the cases
              | BIntegerWordNeg {-# UNPACK #-} !Word -- ^ When we have a negative-valued int whose magnitude fits as a machine-sized word
              | BIntegerZ {-# UNPACK #-} !Integer -- ^ Fallback implementation for all other cases
              | BIntegerZero -- ^ Specific case for 0 (used like 'false')
              | BIntegerOne -- ^ Specific case for 1 (used like 'true')
              | BIntegerTwo -- ^ Specific case for 2 (used for the current version number of the BitTorrent protocol)
  deriving (Show,Eq,Ord,Generic,Typeable)

-- | Given a 'StrictByteString', produce a 'BData' that holds its contents.
mkBBytes :: StrictByteString -> BData
mkBBytes bs
  | SBS.null bs = BBytes BBytesEmpty
  | SBS.length bs <= bbytesLimit = BBytes . BBytesShort $! Sbs.toShort bs
  | otherwise = BBytes . BBytesStrict $! bs
{-# INLINE mkBBytes #-}

-- | Given a 'LazyByteString', produce a 'BData' that holds its contents.
mkBBytesL :: LazyByteString -> BData
mkBBytesL bs
  | LBS.null bs = BBytes BBytesEmpty
  | otherwise = mkBBytes $! LBS.toStrict bs
{-# INLINE mkBBytesL #-}

-- | Given a 'Char' (which may be representable in ASCII or not),
--   produce a 'BData' that holds its value in UTF8.
mkBChar :: Char -> BData
mkBChar c
  | Char.isAscii c = BBytes $! BBytesChar7 c
  | otherwise = mkBString [c]
{-# INLINE mkBChar #-}

-- | Given a 'String', produce a 'BData' that holds its contents as bytes encoded in UTF8.
mkBString :: String -> BData
mkBString []  = BBytes BBytesEmpty
mkBString [c] = mkBChar $! c
mkBString str = mkBBytes $! asUTF8 str
{-# INLINE mkBString #-}

-- | Given an instance of 'ToText', produce a 'BData' that holds its contents as bytes encoded in UTF8.
mkBText :: (ToText a) => a -> BData
mkBText txt = mkBString $! (fromText . toText $! txt)
{-# INLINE mkBText #-}
{-# SPECIALIZE INLINE mkBText :: StrictText -> BData #-}
{-# SPECIALIZE INLINE mkBText :: LazyText -> BData #-}

-- | Given an instance of 'Integral', produce a 'BData' that holds its value.
mkBInteger :: (Integral i) => i -> BData
mkBInteger integral = BInteger $!
  case toInteger integral of
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
    maxWord8 :: Integer
    maxWord8 = toInteger (maxBound :: Word8)
    maxWord :: Integer
    maxWord = toInteger (maxBound :: Word)
{-# INLINE mkBInteger #-}
{-# SPECIALIZE INLINE mkBInteger :: Integer -> BData #-}
{-# SPECIALIZE INLINE mkBInteger :: Int -> BData #-}
{-# SPECIALIZE INLINE mkBInteger :: Word -> BData #-}
{-# SPECIALIZE INLINE mkBInteger :: Word8 -> BData #-}

-- | Given an instance of a 'Foldable', produce a 'BData' that holds its contents.
mkBList :: (Foldable f, Item (f BData) ~ BData) => f BData -> BData
mkBList datas = BList $!
  case F.toList $! datas of
    []   -> BListEmpty
    [x]  -> BListSingle $! x
    rest -> BListSeq $! (Seq.fromList rest)
{-# INLINE mkBList #-}
{-# SPECIALIZE INLINE mkBList :: [BData] -> BData #-}
{-# SPECIALIZE INLINE mkBList :: Seq BData -> BData #-}

-- | Given an instance of a 'MonoFoldable' over 'BData',
--   produce a 'BData' that holds its contents.
--
--   The "O" at the end of this method comes from the "o" that
--   prefixes the 'MonoFoldable' functions.
mkBListO :: (MonoFoldable lst, Element lst ~ BData) => lst -> BData
mkBListO lst = mkBList $! otoList lst
{-# INLINE mkBListO #-} -- TODO What instances should we specialize this on?

-- | Given an instance of a 'Seq', produce a 'BData' that holds its contents.
mkBSeq :: Seq BData -> BData
mkBSeq datas = BList $!
  case toList datas of
    []  -> BListEmpty
    [x] -> BListSingle $! x
    _   -> BListSeq $! datas
{-# INLINE mkBSeq #-}

-- | Given an instance of a 'Map' from 'StrictByteString' to 'BData', produce a 'BData' that holds its contents.
mkBMap :: Map StrictByteString BData -> BData
mkBMap datas = BMap $!
  case toList datas of
    []          -> BMapEmpty
    [(key,val)] -> BMapSingle (Sbs.toShort key) val
    _           -> BMapMap $! Map.mapKeys Sbs.toShort datas
{-# INLINE mkBMap #-}

-- | Given an instance of a 'Map' from 'LazyByteString' to 'BData', produce a 'BData' that holds its contents.
mkBMapL :: Map LazyByteString BData -> BData
mkBMapL = mkBMap . (Map.mapKeys LBS.toStrict)
{-# INLINE mkBMapL #-}

-- | Given an instance of a 'Map' from 'String' to 'BData', produce a 'BData' that holds its contents, with the keys encoded in UTF8.
mkBMapS :: Map String BData -> BData
mkBMapS = mkBMap . (Map.mapKeys asUTF8)
{-# INLINE mkBMapS #-}

-- | Given an instance of a 'Map' from some kind of 'ToText' to 'BData',
--   produce a 'BData' that holds its contents, with the keys encoded in UTF8.
mkBMapT :: (ToText str) => Map str BData -> BData
mkBMapT = mkBMap . (Map.mapKeys asUTF8)
{-# INLINE mkBMapT #-}
{-# SPECIALIZE INLINE mkBMapT :: Map LazyText BData -> BData   #-}
{-# SPECIALIZE INLINE mkBMapT :: Map StrictText BData -> BData #-}

-- | Given a 'BData' that is supposed to contain some bytes, get a
--   'StrictByteString' which contains those bytes. If the 'BData' does not contain
--   bytes, then calls 'fail'.
getBBytes :: (MonadFail m) => BData -> m StrictByteString
getBBytes (BBytes bytes) =
  case bytes of
    BBytesEmpty         -> return mempty
    BBytesShort short   -> return $! Sbs.fromShort short
    BBytesStrict strict -> return strict
getBBytes wrong = fail $ "Did not have bytes when expected: " <> (show wrong)
{-# INLINE getBBytes #-}
{-# SPECIALIZE INLINE getBBytes :: BData -> Maybe StrictByteString #-}
{-# SPECIALIZE INLINE getBBytes :: BData -> IO StrictByteString #-}

-- | Given a 'BData' that is supposed to contain some bytes, get a
--   'LazyByteString' which contains those bytes. If the 'BData' does not contain
--   bytes, then calls 'fail'.
getBBytesL :: (MonadFail m) => BData -> m LazyByteString
getBBytesL = fmap LBS.fromStrict . getBBytes
{-# INLINE getBBytesL #-}
{-# SPECIALIZE INLINE getBBytesL :: BData -> Maybe LazyByteString #-}
{-# SPECIALIZE INLINE getBBytesL :: BData -> IO LazyByteString #-}

-- | Given a 'BData' that is supposed to contain some bytes, get a
--   'String' which contains those bytes, decoded from UTF8. If the
--   'BData' does not contain bytes that can be decoded from UTF8,
--   then calls 'fail'.
getBString :: (MonadFail m) => BData -> m String
getBString bdata = do
  bytes <- getBBytes bdata
  fromText <$> decodeText (UTF8 bytes)
{-# INLINE getBString #-}
{-# SPECIALIZE INLINE getBString :: BData -> Maybe String #-}
{-# SPECIALIZE INLINE getBString :: BData -> IO String #-}

-- | Given a 'BData' that is supposed to contain some bytes, get an
--   instance of 'FromText' which contains those bytes, decoded from
--   UTF8. If the 'BData' does not contain bytes that can be decoded
--   from UTF8, then calls 'fail'.
getBText :: (MonadFail m, FromText str) => BData -> m str
getBText bdata = fromText . toText <$> getBString bdata
{-# INLINE getBText #-}
{-# SPECIALIZE INLINE getBText :: BData -> Maybe StrictText #-}
{-# SPECIALIZE INLINE getBText :: BData -> IO StrictText #-}
{-# SPECIALIZE INLINE getBText :: BData -> Maybe LazyText #-}
{-# SPECIALIZE INLINE getBText :: BData -> IO LazyText #-}

-- | Given a 'BData' that is supposed to contain a list of other
--   'BData' elements, get the elements wrapped in an instance of
--   an 'IsList'. If the data does not contain a list of elements,
--   then calls 'fail'.
getBList :: (MonadFail m, IsList lst, Item lst ~ BData) => BData -> m lst
getBList (BList blist) = return . fromList $!
  case blist of
    BListEmpty      -> []
    (BListSingle x) -> [x]
    (BListSeq seq)  -> toList seq
getBList wrong = fail $ "Did not have a list when expected: " <> (show wrong)
{-# INLINE getBList #-}
{-# SPECIALIZE INLINE getBList :: BData -> Maybe [BData] #-}
{-# SPECIALIZE INLINE getBList :: BData -> IO [BData] #-}
{-# SPECIALIZE INLINE getBList :: BData -> Maybe (Seq BData) #-}
{-# SPECIALIZE INLINE getBList :: BData -> IO (Seq BData) #-}

-- | Given a 'BData' that is supposed to contain an integer,
--   get the value as an 'Integer'. If the data does not contain
--   an integer, then calls 'fail'.
getBInteger :: (MonadFail m) => BData -> m Integer
getBInteger (BInteger i) = return $!
  case i of
    (BIntegerWord w)    -> toInteger w
    (BIntegerWord8 w)   -> toInteger w
    (BIntegerWordNeg w) -> -1 * (toInteger w)
    (BIntegerZ z)       -> z
    BIntegerZero        -> 0
    BIntegerOne         -> 1
    BIntegerTwo         -> 2
getBInteger wrong = fail $ "Did not have an integer when expected: " <> (show wrong)
{-# INLINE getBInteger #-}
{-# SPECIALIZE INLINE getBInteger :: BData -> Maybe Integer #-}
{-# SPECIALIZE INLINE getBInteger :: BData -> IO Integer #-}

-- | Given a 'BData' that is supposed to contain an integer,
--   get the value as some instance of 'Num'. If the data does
--   not contain an integer, then calls 'fail'.
getBNum :: (MonadFail m, Num n) => BData -> m n
getBNum = fmap fromInteger . getBInteger
{-# INLINE getBNum #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Integer  #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Int      #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Word     #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Word8    #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Double   #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Float    #-}
{-# SPECIALIZE INLINE getBNum :: BData -> Maybe Rational #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Integer     #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Int         #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Word        #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Word8       #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Double      #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Float       #-}
{-# SPECIALIZE INLINE getBNum :: BData -> IO Rational    #-}

-- | Given a 'BData' that is supposed to contain a map of bytes onto
--   data elements, return the 'Map' with its contents. If the argument
--   is not a map, then calls 'fail'.
getBMap :: (MonadFail m) => BData -> m (Map StrictByteString BData)
getBMap (BMap bmap) = return $!
  case bmap of
    BMapEmpty            -> Map.empty
    (BMapSingle key val) -> Map.singleton (Sbs.fromShort key) val
    (BMapMap mapmap)     -> Map.mapKeys Sbs.fromShort mapmap
getBMap wrong = fail $ "Did not have a map when expected: " <> (show wrong)
{-# INLINE getBMap #-}
{-# SPECIALIZE INLINE getBMap :: BData -> Maybe (Map StrictByteString BData) #-}
{-# SPECIALIZE INLINE getBMap :: BData -> IO (Map StrictByteString BData) #-}

-- | Given a 'BData' that is supposed to contain a map of bytes onto
--   data elements, return the 'Map' with its contents. If the argument
--   is not a map, then calls 'fail'.
getBMapL :: (MonadFail m) => BData -> m (Map LazyByteString BData)
getBMapL = fmap (Map.mapKeys LBS.fromStrict) . getBMap
{-# INLINE getBMapL #-}
{-# SPECIALIZE INLINE getBMapL :: BData -> Maybe (Map LazyByteString BData) #-}
{-# SPECIALIZE INLINE getBMapL :: BData -> IO (Map LazyByteString BData) #-}

-- | Given a 'BData' that is supposed to contain a map of bytes onto
--   data elements, return the 'Map' with its contents, with the keys having
--   been converted to 'String' instances using the UTF8 encoding. Keys that
--   cannot be converted to UTF8 are silently discarded.  If the argument is
--   not a map, then calls 'fail'.
getBMapS :: (MonadFail m) => BData -> m (Map String BData)
getBMapS bdata = do
  pairs <- Map.toList <$> getBMap bdata
  let strKeyPairs = catMaybes $ (\(k,v) -> (\s -> (s,v)) <$> toUTF8 k) <$> pairs
  return $! Map.fromList strKeyPairs
{-# INLINE getBMapS #-}
{-# SPECIALIZE INLINE getBMapS :: BData -> Maybe (Map String BData) #-}
{-# SPECIALIZE INLINE getBMapS :: BData -> IO (Map String BData) #-}

-- | Given a 'BData' that is supposed to contain a map of bytes onto
--   data elements, return the 'Map' with its contents, with the keys having
--   been converted to 'FromText' instances using the UTF8 encoding. Keys that cannot
--   be converted to UTF8 are silently discarded.  If the argument is not a map,
--   then calls 'fail'.
getBMapT :: (MonadFail m, Ord str, FromText str) => BData -> m (Map str BData)
getBMapT = fmap convertKeys . getBMapS
  where
    convertKeys = Map.mapKeys toTextish
    toTextish = fromText . toText
{-# INLINE getBMapT #-}
{-# SPECIALIZE INLINE getBMapT :: BData -> Maybe (Map LazyText BData) #-}
{-# SPECIALIZE INLINE getBMapT :: BData -> IO (Map LazyText BData) #-}
{-# SPECIALIZE INLINE getBMapT :: BData -> Maybe (Map StrictText BData) #-}
{-# SPECIALIZE INLINE getBMapT :: BData -> IO (Map StrictText BData) #-}

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

-- | Handles the case of being at the end of the file.
hIfEOF :: (MonadIO m) => m a -> (Handle -> m a) -> Handle -> m a
hIfEOF defaultValue f handle = do
  handleClosed <- hIsClosed handle
  handleReadable <- hIsReadable handle
  eofReached <- hIsEOF handle
  if handleClosed || (not handleReadable) || eofReached then
    defaultValue
  else
    f handle
{-# INLINE hIfEOF #-}
{-# SPECIALIZE INLINE hIfEOF :: IO a -> (Handle -> IO a) -> Handle -> IO a #-}

-- | Reads 'BData' of some arbitrary kind from the handle.
--   If the handle is at EOF, it returns an empty map.
hGetBData :: (MonadIO m, MonadFail m) => Handle -> m BData
hGetBData = hIfEOF (return $! BMap BMapEmpty) $ \handle -> do
  nextChar <- hLookAheadChar handle
  case nextChar of
    'i' -> hGetBInteger handle
    'l' -> hGetBList handle
    'd' -> hGetBMap handle
    c   | Char.isDigit c -> hGetBBytes handle
    _   -> fail $ "Badly formed BEncoded data: " <> [nextChar]
{-# INLINEABLE hGetBData #-}
{-# SPECIALIZE hGetBData :: Handle -> IO BData #-}

-- | Reads an integer, failing if it is EOF or not a properly BEncoded integer.
hGetBInteger :: (MonadIO m, MonadFail m) => Handle -> m BData
hGetBInteger = hIfEOF (fail "At EOF when we expected an integer") $ \handle -> do
    signalChar <- liftIO $ IO.hGetChar handle
    case signalChar of
      'i' -> mkBInteger <$> getDigits handle
      _   -> fail $ "Expected the integer start character (i), but got: " <> [signalChar]
  where
    getDigits handle = do
      digitsLbs <- BSB.toLazyByteString <$> getDigits' mempty handle
      digitsStr <- toUTF8 digitsLbs
      case readEither digitsStr of
        Left msg  -> fail $ "Could not read the digits string: " <> digitsStr <> " => " <> msg
        Right num -> return num
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
{-# SPECIALIZE hGetBInteger :: Handle -> IO BData #-}

-- | Reads a list, returning the empty list if it is EOF and failing if it is not a properly BEncoded list.
hGetBList :: (MonadIO m, MonadFail m) => Handle -> m BData
hGetBList = hIfEOF (return $! BList BListEmpty) $ \handle -> do
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
{-# SPECIALIZE hGetBList :: Handle -> IO BData #-}

-- | Gets a map/dictionary from the handle, returning the empty map if it is at EOF and failing if it is not a properly BEncoded dictionary.
--   Note that this implementation is tolerant if the keys are out of order.
hGetBMap :: (MonadIO m, MonadFail m) => Handle -> m BData
hGetBMap = hIfEOF (return $! BMap BMapEmpty) $ \handle -> do
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
          keyBytes <- hGetBBytes handle
          key <- getBBytes keyBytes
          value <- hGetBData handle
          getContents' (Map.insert key value memo) handle
{-# INLINEABLE hGetBMap #-}
{-# SPECIALIZE hGetBMap :: Handle -> IO BData #-}

-- | Gets the bytes of what the spec calls a "string" from the handle, returning a zero-length bytestring if it is at EOF and
--   failing if it is not a properly BEncoded string.
hGetBBytes :: (MonadIO m, MonadFail m) => Handle -> m BData
hGetBBytes = (fmap BBytes) . getBytes
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
{-# SPECIALIZE hGetBBytes ::  Handle -> IO BData #-}

-- | Looks up a value from a 'BData' assuming that it is a map.
lookupBMap :: (MonadFail m) => StrictByteString -> BData -> m BData
lookupBMap key bdata = do
  bmap <- getBMap bdata
  let maybeValue = Map.lookup key bmap
  maybe (fail $ "Could not find key: " <> show key) (return) maybeValue
{-# INLINE lookupBMap #-}
{-# SPECIALIZE INLINE lookupBMap :: StrictByteString -> BData -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMap :: StrictByteString -> BData -> Maybe BData #-}

-- | Looks up a value from a 'BData' assuming that it is a map.
lookupBMapL :: (MonadFail m) => LazyByteString -> BData -> m BData
lookupBMapL key = lookupBMap (LBS.toStrict key)
{-# INLINE lookupBMapL #-}
{-# SPECIALIZE INLINE lookupBMapL :: LazyByteString -> BData -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapL :: LazyByteString -> BData -> Maybe BData #-}

-- | Looks up a value from a 'BData' assuming it is a map whose keys are encoded in UTF8.
--   The key must be a string proper: if you have something that's string-like, use 'lookupBMapT'.
lookupBMapS :: (MonadFail m) => String -> BData -> m BData
lookupBMapS str = lookupBMap (asUTF8 str)
{-# INLINE lookupBMapS #-}
{-# SPECIALIZE INLINE lookupBMapS :: String -> BData -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapS :: String -> BData -> Maybe BData #-}

-- | Looks up a value from a 'BData' assuming that it is a map whose keys are encoded in UTF8.
--   The key can be of any type that supports 'ToText'.
lookupBMapT :: (MonadFail m, ToText a) => a -> BData -> m BData
lookupBMapT str = lookupBMap (asUTF8 str)
{-# INLINE lookupBMapT #-}
{-# SPECIALIZE INLINE lookupBMapT :: LazyText -> BData -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: LazyText -> BData -> Maybe BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: StrictText -> BData -> IO BData #-}
{-# SPECIALIZE INLINE lookupBMapT :: StrictText -> BData -> Maybe BData #-}

-- | Checks to see if the 'BData' is a map containing the given key.
hasKeyBMap :: StrictByteString -> BData -> Bool
hasKeyBMap key bdata = maybe False (isJust . Map.lookup key) (getBMap bdata)
{-# INLINE hasKeyBMap #-}

-- | Checks to see if the 'BData' is a map containing the given key.
hasKeyBMapL :: LazyByteString -> BData -> Bool
hasKeyBMapL = hasKeyBMap . LBS.toStrict
{-# INLINE hasKeyBMapL #-}

-- | Checks to see if the 'BData' is a map containing the given key encoded in UTF8.
hasKeyBMapS :: String -> BData -> Bool
hasKeyBMapS = hasKeyBMap . asUTF8
{-# INLINE hasKeyBMapS #-}

-- | Checks to see if the 'BData' is a map containing the given key encoded in UTF8.
--   The key can be of any type that supports 'ToText'.
hasKeyBMapT :: (ToText a) => a -> BData -> Bool
hasKeyBMapT = hasKeyBMap . asUTF8
{-# INLINE hasKeyBMapT #-}
{-# SPECIALIZE INLINE hasKeyBMapT :: LazyText -> BData -> Bool #-}
{-# SPECIALIZE INLINE hasKeyBMapT :: StrictText -> BData -> Bool #-}

-- | Given a 'Foldable' instance containing tuples of 'StrictByteString' keys 'BData' values,
--   construct a 'BData' of a map containing all this data.
fromPairsBMap :: (Foldable lst, Item (lst (StrictByteString, BData)) ~ (StrictByteString,BData)) =>
  lst (StrictByteString, BData) -> BData
fromPairsBMap = mkBMap . Map.fromList . F.toList
{-# INLINE fromPairsBMap #-}
{-# SPECIALIZE INLINE fromPairsBMap :: [(StrictByteString, BData)] -> BData #-}

-- | Given a 'Foldable' instance containing tuples of 'LazyByteString' keys 'BData' values,
--   construct a 'BData' of a map containing all this data.
fromPairsBMapL :: (Foldable lst, Item (lst (LazyByteString, BData)) ~ (LazyByteString,BData)) =>
  lst (LazyByteString, BData) -> BData
fromPairsBMapL = mkBMapL . Map.fromList . F.toList
{-# INLINE fromPairsBMapL #-}
{-# SPECIALIZE INLINE fromPairsBMapL :: [(LazyByteString, BData)] -> BData #-}

-- | Given a 'MonoFoldable' instance containing tuples of 'StrictByteString' keys 'BData' values,
--   construct a 'BData' of a map containing all this data.
--
--   The "O" at the end of this method comes from the "o" that prefixes the 'MonoFoldable' functions.
fromPairsBMapO :: (MonoFoldable lst, Element lst ~ (StrictByteString,BData)) => lst -> BData
fromPairsBMapO = fromPairsBMap . otoList
{-# INLINE fromPairsBMapO #-}

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
unpack = build . lazy
  where
    build (BBytes BBytesEmpty) = BSB.string7 "0:"
    build (BBytes (BBytesChar7 c)) = BSB.string7 "1:" <> BSB.char7 c
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
    build (BList (BListSeq bdatas)) = BSB.char7 'l' <> (foldr (\cur rest -> build cur <> rest) mempty bdatas) <> BSB.char7 'e'
    build (BMap BMapEmpty) = BSB.string7 "de"
    build (BMap (BMapSingle sbs bdata)) = BSB.char7 'd' <> (buildKey sbs) <> (build bdata) <> BSB.char7 'e'
    build (BMap (BMapMap bmap)) = -- Ordering must be "binary ascending key", and that's guaranteed by 'Map.foldrWithkey'
      BSB.char7 'd'
      <> (Map.foldrWithKey (\key val rest -> buildKey key <> build val <> rest) mempty bmap)
      <> BSB.char7 'e'
    buildKey = build . BBytes . BBytesShort
{-# INLINEABLE unpack #-}

-- | Get the key/value pairs out of a 'BData' which is presumed to be a map.  If it is not a
--   map, then calls 'fail'.
toPairsBMap :: (MonadFail m) => BData -> m [(StrictByteString, BData)]
toPairsBMap = (fmap Map.toList) . getBMap
{-# INLINE toPairsBMap #-}
{-# SPECIALIZE INLINE toPairsBMap :: BData -> Maybe [(StrictByteString, BData)] #-}
{-# SPECIALIZE INLINE toPairsBMap :: BData -> IO [(StrictByteString, BData)]    #-}

-- | Get the key/value pairs out of a 'BData' which is presumed to be a map.  If it is not a
--   map, then calls 'fail'.
toPairsBMapL :: (MonadFail m) => BData -> m [(LazyByteString, BData)]
toPairsBMapL = fmap (fmap $ \(sbs,bdata) -> (LBS.fromStrict sbs, bdata)) . toPairsBMap
{-# INLINE toPairsBMapL #-}
{-# SPECIALIZE INLINE toPairsBMapL :: BData -> Maybe [(LazyByteString, BData)] #-}
{-# SPECIALIZE INLINE toPairsBMapL :: BData -> IO [(LazyByteString, BData)]    #-}

-- | Get the key/value pairs out of a 'BData' which is presumed to be a map whose keys
--   are convertible to UTF8. If it is not a map, then calls 'fail'. If a key cannot
--   be converted to UTF8, it is silently discarded.
toPairsBMapS :: (MonadFail m) => BData -> m [(String, BData)]
toPairsBMapS bdata = do
  pairs <- toPairsBMap bdata
  return $ do
    (sbs,bd) <- pairs
    case toUTF8 sbs of
      Nothing  -> []
      Just str -> return (str,bd)
{-# INLINE toPairsBMapS #-}
{-# SPECIALIZE INLINE toPairsBMapS :: BData -> Maybe [(String, BData)] #-}
{-# SPECIALIZE INLINE toPairsBMapS :: BData -> IO [(String, BData)]    #-}
