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
  }

-- | The representation of a file tree in the info dictionary
data InfoDictFileTree =
  InfoDir StrictText MetaInfoFileTree -- ^ Single directory pointing to a tree
  | InfoDirs (Map StrictText MetaInfoFileTree) -- ^ Multiple sibling directories pointing to a tree
  | InfoFile StrictText Word64 StrictText -- ^ A file with its name, length, and pieces root

-- | The representation of the piece layers in the meta info file (itself represented by 'MetaInfoFile').
type MetaInfoPieceLayers = ()

-- | A representation of the meta info file
data MetaInfoFile = MetaInfoFile
  { mifAnnounce :: URI
  , mifInfoDict :: MetaInfoDict
  , mifPieceLayers :: MetaInfoPieceLayers
  }

-- | Provides the infohash for an 'InfoDict', which is the identifier for the Torrent.
infohash :: InfoDict -> Digest SHA256
infohash dict = hash . pack $ toBMap dict

-- | Wrapper around an info hash to denote that it's the full thing (not truncated).
newtype Infohash = Infohash StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord)

-- | Provides the infohash as a 'StrictByteString'.
infohashSBS :: InfoDict -> StrictByteString
infohashSBS = Infohash . SBS.pack . unpack . infohash

-- | Wrapper around an info hash to denote that it's truncated (not the full thing)
newtype ShortInfohash = ShortInfohash StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord)

-- | The 20-byte version of 'infohash', used in certain cases.
shortInfohash :: InfoDict -> StrictByteString
shortInfohash = ShortInfohash . SBS.pack . List.take 20 . unpack . infohash

-- | The peer id, which is supposed to be a string of length 20.
newtype PeerId = PeerId StrictByteString
  deriving (ByteArrayAccess,Show,Eq,Ord)

-- | The particular events that can create a tracker request.
data TrackerEvent
  = DownloadStarted -- ^ "started"
  | DownloadCompleted  -- ^ "completed"
  | DownloadAborted  -- ^ "stopped"
  | Heartbeat -- ^ "empty"

-- | The mapping of tracker events to the strings used to represent them
--   in the BitTorrent protocol.
trackerEventProtocolCode :: TrackerEvent -> String
trackerEventProtocolCode DownloadStarted   = "started"
trackerEventProtocolCode DownloadCompleted = "completed"
trackerEventProtocolCode DownloadAborted   = "stopped"
trackerEventProtocolCode Heartbeat         = "empty"

-- | Represents the request to a tracker
data TrackerRequest = TrackerRequest
  { trInfohash :: ShortInfohash -- ^ The infohash to request
  , trPeerId :: PeerId -- ^ The peer's ID (self-assigned)
  , trPeerIP :: Maybe IP -- ^ The peer's IP address, if it's known
  , trPort :: Word16 -- ^ The port number that the peer is listening to
  , trUploaded :: Integer -- ^ The total number of bytes that have been uploaded
  , trDownloaded :: Integer -- ^ The total number of bytes that have been downloaded
  , trLeft :: Integer -- ^ The total number of bytes that are left
  , trEvent :: TrackerEvent -- ^ The event that lead to this value
  }


