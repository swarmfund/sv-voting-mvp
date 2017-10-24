module SecureVote.Utils.Array where
  

import Prelude

import Data.Array ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)


fromList :: forall a. L.List a -> Array a
fromList ls = do
    let headM = L.head ls
    case headM of
        Nothing -> []
        Just a -> a : (fromMaybe ([]) $ fromList <$> (L.tail ls))