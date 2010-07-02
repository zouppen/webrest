module GitInternal where

import System.IO
import System.Exit
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Control.Monad (liftM)
import System.Process
import Helpers
import GitTypes

-- |Mapper from letter to Git file status code.
toStatus = lookupM [('M',InPlaceEdit)
                   ,('C',CopyEdit)
                   ,('R',RenameEdit)
                   ,('A',Create)
                   ,('D',Delete)
                   ,('U',Unmerged)
                   ] "Unrecognized git status letter."

-- |Parser for maybe parsing any positive 10-base number.
maybeNumber :: (Num a, Read a) => GenParser Char () (Maybe a)
maybeNumber = (liftM (Just . read) $ many1 digit) <|> return Nothing

nul = char '\NUL'

gitPath = "git"

-- |Parses diff tree.
diffTreeParser :: GenParser Char () [DiffInfo]
diffTreeParser = do
  list <- many diffTreeLine
  eof
  return list

-- |Parses a single diff tree line.
diffTreeLine :: GenParser Char () DiffInfo
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
  score <- maybeNumber
  nul
  file <- manyTill anyChar nul

  return $ DiffInfo modeSrc modeDst hashSrc hashDst status score file

-- |Runs git comand on given repository with given arguments. Returns
-- Left in case of an error and Right in case off success. This
-- function is used as a plumbing for higher level functions.
runGit :: Maybe String -> [String] -> IO B.ByteString
runGit repo args = do
  process <- createProcess cp
  contents <- B.hGetContents $ outH process
  errors <- hGetContents $ errH process
  ret <- waitForProcess $ processH process
  case ret of
    ExitSuccess   -> return contents
    ExitFailure _ -> fail errors
  
  where cp = CreateProcess {
                     cmdspec = RawCommand gitPath args
                   , cwd     = repo
                   , env     = Nothing
                   , std_in  = Inherit
                   , std_out = CreatePipe
                   , std_err = CreatePipe
                   , close_fds = False
             }
        processH (_,_,_,x) = x
        outH (_,Just x,_,_) = x
        errH (_,_,Just x,_) = x
