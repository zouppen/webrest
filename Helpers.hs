module Helpers where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

-- |Returns a getter for a map which throws given error message if
-- |element is not found in the map. This is used to get nicer error
-- |messages than "element not in the map" to the user. If you don't
-- |like having error function you can just pass @const "message"@.
findWithErrorF :: (Ord k) => (k -> String) -> M.Map k b -> k -> b
findWithErrorF f map k = M.findWithDefault (error $ f k) k map

-- |Can be used to prepare a conversion function from ByteString to
-- |any type. In case of failing, it prints a nice error message.
friendlyBSConvert :: String -> [(B.ByteString, a)] -> B.ByteString -> a
friendlyBSConvert msg list k = findWithErrorF e (M.fromList list) k
    where e x = concat [msg," Unknown key: ", B.unpack x] 

-- |Creates ByteString to anything convertor where ordinary strings
-- |can be used as keys.
bsConvertor :: [(String, a)] -> String -> B.ByteString -> a
bsConvertor list msg = friendlyBSConvert msg $ map toBS list 
    where toBS (a,b) = (B.pack a,b)

-- |Looks up a value from key-value list and returns result in a monad
-- |of your disposal. This is efficient only on small lists. Feel free
-- |to implement this with Map if you need more.
lookupM :: (Eq k, Monad m) => [(k, a)] -> String -> k -> m a
lookupM list errorMsg k = case lookup k list of
                            Just a -> return a
                            Nothing -> fail errorMsg
