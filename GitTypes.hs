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

data Options = Options {
      editDetection :: EditDetection
    , recurse       :: Bool
    } deriving (Show)

defaultOptions = Options {
                   editDetection = InPlace
                 , recurse = True
                 }
