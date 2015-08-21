module Network.IPFS where

import Control.Applicative ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import qualified Network.HTTP.Conduit as HTTP
import Text.ProtocolBuffers.WireMessage (messageGet)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Network.IPFS.API as API
import qualified Network.IPFS.MerkleDAG.PBNode as PBN
import qualified Network.IPFS.MerkleDAG.PBLink as PBL

type Hash = B.ByteString -- TODO use multihash library
type Data = B.ByteString

data Object = Object { hash :: Hash
                     , payload :: Data
                     , links :: [(String, Object)]
                     } deriving (Show)

getPBNode :: HTTP.Manager -> String -> Hash -> IO PBN.PBNode
getPBNode manager endpoint digest = do
    resp <- API.call manager endpoint
        ["object", "get"] [("encoding", "protobuf")]
        [C.unpack $ B58.encodeBase58 B58.bitcoinAlphabet digest]
    return $ case messageGet resp of
        Right (node, _) -> node
        Left err -> error err

getObject :: HTTP.Manager -> String -> Hash -> IO Object
getObject manager endpoint = resolve
  where resolve digest = do
            pbnode <- getPBNode manager endpoint digest
            let links' = toList $ PBN.links pbnode
                names = uToString . fromJust . PBL.name <$> links'
                data' = BL.toStrict . fromJust $ PBN.data' pbnode
            children <- mapM resolveLink links'
            return (Object digest data' $ zip names children)
        resolveLink = resolve . BL.toStrict . fromJust . PBL.hash
