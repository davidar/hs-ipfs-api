{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base58 as B58
import Data.Maybe (fromJust)
import Network.IPFS
import qualified Network.IPFS.API as API

main :: IO ()
main = do
    ipfs <- API.init "http://localhost:5001/"
    let digest = fromJust $ B58.decodeBase58 B58.bitcoinAlphabet "QmPXME1oRtoT627YKaDPDQ3PwA8tdP9rWuAAweLzqSwAWT"
    obj <- ipfs `getObject` digest
    print obj
