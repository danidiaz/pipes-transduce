module Main where

import Test.DocTest

main :: IO ()
main = doctest 
    [
        "src/Pipes/Transduce.hs"
    ,   "src/Pipes/Transduce/ByteString.hs"
    ,   "src/Pipes/Transduce/Text.hs"
    ]
