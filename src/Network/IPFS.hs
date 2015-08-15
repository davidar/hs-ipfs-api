{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    OverloadedStrings #-}

module Network.IPFS where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base58 as B58
import Data.Maybe
import Data.Foldable (toList)
import Network.HTTP.Conduit
import Network.HTTP.Types.URI
import Blaze.ByteString.Builder (toByteString)
import Text.ProtocolBuffers.WireMessage (messageGet)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Network.IPFS.MerkleDAG.PBNode as PBN
import qualified Network.IPFS.MerkleDAG.PBLink as PBL

type Hash = B.ByteString -- TODO use multihash library
type Data = B.ByteString

data Object = Object { hash :: Hash
                     , payload :: Data
                     , links :: [(String, Object)]
                     } deriving (Show)

getPBNode :: Manager -> String -> Hash -> IO PBN.PBNode
getPBNode manager endpoint digest = do
    result <- messageGet . responseBody <$> httpLbs req manager
    return $ case result of
        Right (node, _) -> node
        Left err -> error err
  where b58 = B58.encodeBase58 B58.bitcoinAlphabet digest
        cmd = ["object", "get"]
        query = [("encoding", Just "protobuf"), ("arg", Just b58)]
        req = (fromJust $ parseUrl endpoint) {
            path = toByteString . encodePathSegments $ ["api", "v0"] ++ cmd,
            queryString = renderQuery True query }

getObject :: Manager -> String -> Hash -> IO Object
getObject manager endpoint = resolve
  where resolve digest = do
            pbnode <- getPBNode manager endpoint digest
            let links' = toList $ PBN.links pbnode
                names = uToString . fromJust . PBL.name <$> links'
                data' = BL.toStrict . fromJust $ PBN.data' pbnode
            children <- mapM resolveLink links'
            return (Object digest data' $ zip names children)
        resolveLink = resolve . BL.toStrict . fromJust . PBL.hash
