module GitInternal where

import System.IO
import System.Exit
import System.Process
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
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

toType = lookupM [("blob",Blob)
                 ,("tree",Tree)
                 ] "Unrecognized git file type."

-- |Parser for maybe parsing any positive 10-base number. That means
-- if parsing can't be done, no input is consumed and Nothing is
-- returned.
maybeIntegral :: (Read a, Integral a) => GenParser Char () (Maybe a)
maybeIntegral = liftM Just integral <|> return Nothing

-- |Ordinary integral number.
integral :: (Read a, Integral a) => GenParser Char () a
integral = do
  a <- many1 digit
  return $ read a

nul = char '\NUL'

-- |Reads file size in git format.
size :: (Read a, Integral a) => GenParser Char () (Maybe a)
size = do
  spaces
  liftM Just integral <|> noSize

-- |Tries to read "no size", that appears in the size displayed for trees.
noSize = do
  char '-'
  return Nothing

-- |Useful in situation where it is known beforehand that one
-- shouldn't parse if the condition is False. In both cases no input
-- is consumed.
nothingOrFail :: (Monad m) => Bool -> m (Maybe a)
nothingOrFail False = return Nothing
nothingOrFail True = fail "Design error."

gitPath = "git"

-- |Parses git listings.
gitLines :: GenParser Char () a   -- ^ A parser for reading a git record.
         -> GenParser Char () [a] -- ^ Returns: A parser for whole output.
gitLines line = do
  list <- many line
  eof
  return list

-- |Parses a single line of git diff-tree.
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
  score <- maybeIntegral
  nul
  fileSrc <- manyTill anyChar nul

  -- Git binary output is not perfectly defined. Why there are no two
  -- successive NULs if there is not dstFile?  Now we need conditional
  -- processing which is ugly.
  fileDst <- nothingOrFail (status `elem` [CopyEdit,RenameEdit]) <|>
             liftM Just (manyTill anyChar nul)

  return $ DiffInfo modeSrc modeDst hashSrc hashDst status score fileSrc fileDst
  
-- |Reads a single line of git ls-tree.
lsTreeLine :: Bool                     -- ^ Is file size included?
           -> GenParser Char () LsInfo -- ^ Returns: Parser
lsTreeLine hasSize = do
  mode <- count 6 digit
  space
  fileType <- manyTill lower space >>= toType
  hash <- count 40 hexDigit
  fileSize <- nothingOrFail hasSize <|> size 
  tab
  fileName <- manyTill anyChar nul
  
  return $ LsInfo mode fileType hash fileSize fileName

-- |Runs git command on given repository with given arguments. Fails in
-- case of an error. This function is used as a plumbing for higher
-- level functions.
runGit :: Maybe String    -- ^ Path to repository, Nothing if current dir.
       -> [Maybe String]  -- ^ List of Just parameters.
       -> IO B.ByteString -- ^ Returns: Git output.
runGit repo args = do
  process <- createProcess cp
  contents <- B.hGetContents $ outH process
  errors <- hGetContents $ errH process
  ret <- waitForProcess $ processH process
  case ret of
    ExitSuccess   -> return contents
    ExitFailure _ -> fail errors
  
  where cp = CreateProcess {
                     cmdspec = RawCommand gitPath $ catMaybes args
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

-- |Runs git and parses its output.
runAndParseGit :: GenParser Char () a  -- ^ Parser for results.
               -> Maybe String         -- ^ Path to repository.
               -> [Maybe String]       -- ^ Parameters for git.
               -> IO a                 -- ^ Returns parser output.
runAndParseGit parser wd params = do
  gitOut <- runGit wd params
  case parse parser "(git)" gitOut of
    Left e -> fail $ show e
    Right a -> return a

-- Now comes some option generators.

gitOptRecurse :: Options -> Maybe String
gitOptRecurse o = case recurse o of
                      True -> Just "-r"
                      False -> Nothing

gitOptEdit :: Options -> Maybe String
gitOptEdit o = case editDetection o of
                   InPlace -> Nothing
                   Rename  -> Just "-R"
                   Copy    -> Just "-C"

gitOptSize :: Options -> Maybe String
gitOptSize o = case showSizes o of
               True -> Just "-l"
               False -> Nothing
