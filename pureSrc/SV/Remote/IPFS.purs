module SV.Light.IPFS where

import SV.Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Uncurried (EffFn3, runEffFn3)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import IPFS (IPFS, IPFSEff)
import IPFS.Block (GetResult, Cid, get)


data Protocol = HTTP | HTTPS
instance showProt :: Show Protocol where
    show HTTP = "http"
    show HTTPS = "https"
derive instance eqProtocol :: Eq Protocol


ipfsRef :: Ref IPFS
ipfsRef = unsafePerformEff $ newRef $ unsafePerformEff $ connect "ipfs.infura.io" 5001 HTTPS


setNewIpfs :: forall e. String -> Int -> Protocol -> Eff (ref :: REF, ipfs :: IPFSEff | e) Unit
setNewIpfs h p protocol = writeRef ipfsRef =<< connect h p protocol


foreign import connectImpl :: ∀ eff. EffFn3 (ipfs :: IPFSEff | eff) String Int String IPFS
connect :: forall e. String -> Int -> Protocol -> Eff (ipfs :: IPFSEff | e) IPFS
connect domain port protocol = runEffFn3 connectImpl domain port (show protocol)

getBlock :: forall e. Cid -> Aff (ref :: REF, ipfs :: IPFSEff | e) GetResult
getBlock c = do
    ipfs <- liftEff $ readRef ipfsRef
    get ipfs c
