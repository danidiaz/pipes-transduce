module Main where

import Test.DocTest

main :: IO ()
main = doctest 
    [
        "src/Pipes/Transduce/Internal.hs"
    ,   "src/Pipes/Transduce/ByteString.hs"
    ,   "src/Pipes/Transduce/Text.hs"
    ]
