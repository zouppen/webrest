module Git where

import Text.Parsec (parse)
import GitTypes
import GitInternal

-- |Compares the content and mode of blobs found via two tree
-- |objects. Uses git-diff-tree internally.
diffTree :: Options        -- ^ Options.
         -> Maybe FilePath -- ^ Path to repository or Nothing if current dir.
         -> String         -- ^ Source tree-ish (revision number etc.).
         -> String         -- ^ Destination tree-ish.
         -> Maybe FilePath -- ^ Git filesystem path to diff.
         -> IO [DiffInfo]  -- ^ Returns differece list
diffTree o repo oldRev newRev path = do
  runAndParseGit diffTreeParser repo params
    where params = [ Just "diff-tree",Just "-z"
                   , gitOptEdit o, gitOptRecurse o
                   , Just oldRev, Just newRev
                   , path
                   ]

