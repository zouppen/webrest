module Git where

import Text.Parsec (parse)
import GitTypes
import GitInternal

-- |Compares the content and mode of blobs found via two tree
-- |objects. Uses git-diff-tree.
diffTree :: Maybe FilePath -> String -> String -> IO [DiffInfo]
diffTree repo oldRev newRev = do
  gitOut <- runGit repo ["diff-tree","-M","-C","-r","-z",oldRev,newRev]
  case parse diffTreeParser "(git)" gitOut of
    Left e -> fail $ show e
    Right a -> return a

