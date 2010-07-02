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
