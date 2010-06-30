module Git where

import Text.Parsec
import Helpers

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
    , file    :: GitFile
    } deriving (Show)

data Status = InPlaceEdit | CopyEdit | RenameEdit | Create | Delete | Unmerged
              deriving (Show,Eq)

toStatus = lookupM [('M',InPlaceEdit)
                   ,('C',CopyEdit)
                   ,('R',RenameEdit)
                   ,('A',Create)
                   ,('D',Delete)
                   ,('U',Unmerged)
                   ] "Unrecognized git status letter."

-- |Used to get Nothing if parsing empty string.
safeRead [] = Nothing
safeRead x = Just $ read x

-- |Parses diff tree.
diffTree :: Parsec String () [DiffInfo]
diffTree = do
  list <- many diffTreeLine
  eof
  return list

-- |Parses a single diff tree line.
diffTreeLine :: Parsec String () DiffInfo
diffTreeLine = do
  char ':'
  modeSrc <- count 6 digit
  space
  modeDst <- count 6 digit
  space
  hashSrc <- count 40 hexDigit
  space
  hashDst <- count 40 hexDigit
  space
  status <- upper >>= toStatus
  score <- parsecMap safeRead $ many digit
  many1 space
  file <- manyTill anyChar newline

  return $ DiffInfo modeSrc modeDst hashSrc hashDst status score file

-- runInteractiveProcess
--   :: FilePath		-- ^ Filename of the executable
--   -> [String]		-- ^ Arguments to pass to the executable
--   -> Maybe FilePath		-- ^ Optional path to the working directory
--   -> Maybe [(String,String)]	-- ^ Optional environment (otherwise inherit)
--   -> IO (Handle,Handle,Handle,ProcessHandle)
