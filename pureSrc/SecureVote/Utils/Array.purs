module SecureVote.Utils.Array where


import SV.Prelude

import Data.Array (drop, filter, take, (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)


fromList :: forall a. L.List a -> Array a
fromList ls = do
    let headM = L.head ls
    case headM of
        Nothing -> []
        Just a -> a : (fromMaybe ([]) $ fromList <$> (L.tail ls))


chunk :: forall a. Int -> Array a -> Array (Array a)
chunk size [] = []
chunk size as = [take size as] <> chunk size (drop size as)


onlyJust :: forall a. Array (Maybe a) -> Array a
onlyJust mas = unsafePartial fromJust $ sequence $ filter isJust mas
