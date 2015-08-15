{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Base58 as B58
import Data.Maybe
import Network.HTTP.Conduit
import Network.IPFS

main :: IO ()
main = do
    let digest = fromJust $ B58.decodeBase58 B58.bitcoinAlphabet "QmPXME1oRtoT627YKaDPDQ3PwA8tdP9rWuAAweLzqSwAWT"
    manager <- newManager tlsManagerSettings
    let get = getObject manager "http://localhost:5001/"
    obj <- get digest
    print obj
