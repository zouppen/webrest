module Rest where

-- rst2xml is deterministic despite runProcess.
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import System.Exit
import qualified Data.ByteString as B
import Text.ParserCombinators.Parsec
import Text.XML.HXT.Parser.XmlParsec

--restToXML :: B.ByteString -> XmlTree
--restToXML bs = unsafePerformIO restToXML'

rst2xmlPath = "rst2xml"

--restToXML' :: B.ByteString -> IO a --XmlTree
restToXML' bs = do
  rawXML <- externalMap rst2xmlPath [] bs
  return $ parse document rawXML


-- |Runs command on with given arguments. Fails in case of an
-- error. This function is used as a plumbing for higher level
-- functions.
externalMap :: String          -- ^ Command to run.
            -> [String]        -- ^ List of parameters.
            -> B.ByteString    -- ^ Input.
            -> IO B.ByteString -- ^ Returns: Standard output.
externalMap cmd args input = do
  process <- createProcess cp
  B.hPut (inH process) input
  hClose (inH process) -- Won't stop otherwise.
  contents <- B.hGetContents $ outH process
  errors <- hGetContents $ errH process
  ret <- waitForProcess $ processH process
  case ret of
    ExitSuccess   -> return contents
    ExitFailure _ -> fail errors
  
  where cp = CreateProcess {
                     cmdspec = RawCommand cmd args
                   , cwd     = Nothing
                   , env     = Nothing
                   , std_in  = CreatePipe
                   , std_out = CreatePipe
                   , std_err = CreatePipe
                   , close_fds = False
             }
        processH (_,_,_,x) = x
        inH  (Just x,_,_,_) = x
        outH (_,Just x,_,_) = x
        errH (_,_,Just x,_) = x
