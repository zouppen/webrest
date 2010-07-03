module GitTypes where

type GitMode = String
type GitHash = String
type GitFile = String

data DiffInfo = DiffInfo {
      modeSrc :: GitMode
    , modeDst :: GitMode
    , hashSrc :: GitHash
    , hashDst :: GitHash
    , status  :: Status
    , score   :: Maybe Integer
    , srcFile :: GitFile
    , dstFile :: Maybe GitFile
    } deriving (Show)

data Status = InPlaceEdit | CopyEdit | RenameEdit | Create | Delete | Unmerged
              deriving (Show,Eq)

data EditDetection = InPlace | Rename | Copy  deriving (Show)

data Type = Tree | Blob  deriving (Show)

data LsInfo = LsInfo {
      mode     :: GitMode
    , fileType :: Type
    , hash     :: GitHash
    , fileSize :: Maybe Integer
    , fileName :: GitFile
    }

data Options = Options {
      editDetection :: EditDetection -- ^ Detection of non-in-place edits.
    , recurse       :: Bool          -- ^ Recurse into subdirectories.
    , showSizes     :: Bool          -- ^ Show file sizes where available.
    } deriving (Show)

defaultOptions = Options {
                   editDetection = InPlace
                 , recurse = True
                 , showSizes = False
                 }
