module Git where

import Text.Parsec (parse)
import GitTypes
import GitInternal
import qualified Data.ByteString as B

-- |Compares the content and mode of blobs found via two tree
-- |objects. Uses git-diff-tree internally.
diffTree :: Options        -- ^ Options.
         -> Maybe FilePath -- ^ Path to repository or Nothing if current dir.
         -> String         -- ^ Source tree-ish (revision number etc.).
         -> String         -- ^ Destination tree-ish.
         -> Maybe FilePath -- ^ Git filesystem path to diff.
         -> IO [DiffInfo]  -- ^ Returns difference list.
diffTree o repo oldRev newRev path = do
  runAndParseGit (gitLines diffTreeLine) repo params
    where params = [ Just "diff-tree",Just "-z"
                   , gitOptEdit o, gitOptRecurse o
                   , Just oldRev, Just newRev
                   , path
                   ]

-- |Lists the files contained in a given tree. Lists the file sizes
-- too, if that option is present. Uses git ls-tree internally.
lsTree :: Options        -- ^ Options.
       -> Maybe FilePath -- ^ Path to repository or Nothing if current dir.
       -> String         -- ^ Tree-ish (revision number etc.).
       -> Maybe FilePath -- ^ Git filesystem path to list.
       -> IO [LsInfo]    -- ^ Returns file listing
lsTree o repo rev path = do
  runAndParseGit (gitLines (lsTreeLine (showSizes o))) repo params
    where params = [ Just "ls-tree",Just "-z"
                   , gitOptSize o, gitOptRecurse o
                   , Just rev
                   , path
                   ]

-- |Loads given file from git repository. Uses git cat-tree internally.
catFile :: Options         -- ^ Options. Ignored at the moment.
        -> Maybe FilePath  -- ^ Path to repository or Nothing if current dir.
        -> String          -- ^ Tree-ish (revision number etc.).
        -> FilePath        -- ^ Git filesystem object to cat.
        -> IO B.ByteString -- ^ Returns: File contents.
catFile o repo rev path = do
  runGit repo params
  where params = [ Just "cat-file", Just "blob",
                   Just (concat [rev,":",path])
                 ]
