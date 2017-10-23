module SecureVote.Utils.Monads where 


import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)

mToE :: forall a. String -> Maybe a -> Either String a 
mToE err m = maybe (Left err) Right m 


