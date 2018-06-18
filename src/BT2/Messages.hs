{-# LANGUAGE NamedFieldPuns #-}

module BT2.Messages ( module BT2.Messages ) where

import           BT2.BEncode
import           Crypto.Hash            ( Digest, hash )
import           Crypto.Hash.Algorithms ( SHA256 )
import qualified Data.ByteString        as SBS
import qualified Data.List              as List
import           Net.IP
import           RFC.Data.URI
import           RFC.Prelude

-- | The representation of an info dictionary
data InfoDict = InfoDict
  { idictName :: StrictText -- ^ The (purely advisory) name for the torrent
  , idictPieceLength :: Word64 -- ^ The length of pieces
  , idictMetaVersion :: Word8 -- ^ The specific version (should be 2). If not provided, it is 1.
  , idictFileTree :: MetaInfoFileTree -- ^ The specifc file trees.
  } deriving (Eq,Ord,Show,Generic,Typeable)

instance ToBMap InfoDict where
  toBMap InfoDict{idictName,idictPieceLength,idictMetaVersion,idictFileTree} =
    toBMap $ Map.fromList
      [ ("name",toBData $ toBBytes idictName)
      , ("piece length",toBData $ toBInteger idictPieceLength)
      , ("meta version",toBData $ toBInteger metaVersion)
      , ("file tree",toBData $ toBMap idictFileTree)
      ]
  {-# INLINEABLE toBMap #-}

-- | The representation of a file tree in the info dictionary
data InfoDictFileTree =
  InfoDir StrictText !InfoDictFileTree -- ^ Single directory pointing to a tree
  | InfoDirs !(Map StrictText InfoDictFileTree) -- ^ Multiple sibling directories pointing to a tree
  | InfoFile StrictText Word64 StrictText -- ^ A file with its name, length, and pieces root
  deriving (Eq,Ord,Show,Generic,Typeable,Enum,Bounded)

instance ToBMap InfoDictFileTree where
  toBMap (InfoDir dirName dirContents) = toBMap $ Map.singleton dirName $ toBData $ toBMap dirContents
  toBMap (InfoDirs dirMap)             = toBMap $ Map.map (toBData . toBMap)
  toBMap (InfoFile fileName length pieceRoot) =
    toBMap $
      Map.singleton name $
        Map.singleton "" $
          Map.fromList
            [ ("length", toBData . toBInteger $ length)
            , ("pieces root", toBData . toBBytes pieceRoot)
            ]
  {-# INLINEABLE toBMap #-}

-- | The representation of the piece layers in the meta info file (itself represented by 'MetaInfoFile').
newtype MetaInfoPieceLayers = MetaInfoPieceLayers [Digest SHA2_256] deriving (Eq,Ord,Show,Generic,Typeable)

-- | A representation of the meta info file
data MetaInfoFile = MetaInfoFile
  { mifAnnounce :: URI
  , mifInfoDict :: MetaInfoDict
  , mifPieceLayers :: MetaInfoPieceLayers
  } deriving (Eq,Ord,Show,Generic,Typeable)

-- | Provides the infohash for an 'InfoDict', which is the identifier for the Torrent.
infohash :: InfoDict -> Infohash
infohash dict = Infohash . SBS.pack . unpack . hash . pack $ toBMap dict
{-# INLINEABLE Infohash #-}

-- | Wrapper around an info hash to denote that it's the full thing (not truncated).
newtype Infohash = Infohash StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord,Generic,Typeable)

-- | Wrapper around an info hash to denote that it's truncated (not the full thing)
newtype ShortInfohash = ShortInfohash StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord,Generic,Typeable)

-- | The 20-byte version of 'infohash', used in certain cases.
shortInfohash :: InfoDict -> StrictByteString
shortInfohash = ShortInfohash . SBS.take 20 . extract . infohash
  where
    extract (Infohash sbs) = sbs
{-# INLINE shortInfohash #-}

-- | The peer id, which is supposed to be a string of length 20.
newtype PeerId = PeerId StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord,Generic,Typeable)

-- | The particular events that can create a tracker request.
data TrackerEvent
  = DownloadStarted -- ^ "started"
  | DownloadCompleted  -- ^ "completed"
  | DownloadAborted  -- ^ "stopped"
  | Heartbeat -- ^ "empty"
  deriving (Eq,Ord,Show,Enum,Bounded,Generic,Typeable)

-- | The mapping of tracker events to the strings used to represent them
--   in the BitTorrent protocol.
trackerEventProtocolCode :: TrackerEvent -> String
trackerEventProtocolCode DownloadStarted   = "started"
trackerEventProtocolCode DownloadCompleted = "completed"
trackerEventProtocolCode DownloadAborted   = "stopped"
trackerEventProtocolCode Heartbeat         = "empty"
{-# INLINE trackerEventProtocolCode #-}

-- | Represents the request to a tracker
data TrackerRequest = TrackerRequest
  { trInfohash :: ShortInfohash -- ^ The infohash to request
  , trPeerId :: PeerId -- ^ The peer's ID (self-assigned)
  , trPeerIP :: Maybe IP -- ^ The peer's IP address, if it's known
  , trPort :: Word16 -- ^ The port number that the peer is listening to
  , trUploaded :: Word -- ^ The total number of bytes that have been uploaded
  , trDownloaded :: Word -- ^ The total number of bytes that have been downloaded
  , trLeft :: Word -- ^ The total number of bytes that are left
  , trEvent :: TrackerEvent -- ^ The event that lead to this value
  } deriving (Show,Eq,Ord,Generic,Typeable)

-- | Represents the response from a tracker
data TrackerResponse
  = TrackerFailure StrictText  -- ^ A failure with its reason
  | TrackerSuccess Word [(PeerId,IP,Word16)] -- ^ A successful response with the ping interval and the peer list
