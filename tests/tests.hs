{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (splitAt,lines,words)
import Data.Char
import Data.String hiding (lines,words)
import Data.Monoid
import Data.Bifunctor
import qualified Data.List (intersperse,splitAt)
import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent (threadDelay)

import Pipes
import Pipes.Transduce
import qualified Pipes.Transduce.Text as PT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Control.Foldl as L

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        testMissingDataInCombined
    ]

testMissingDataInCombined :: TestTree
testMissingDataInCombined = testCase "collectStdoutStderrAsByteString" $ do
    r <- fold2 combinedLines producer1 producer2 
    case r of
        ("aa\nbb\ncc\ndd\n",_,_) -> return ()
        _ -> assertFailure $ "did not expect" ++ show r
    where
    producer1 = do
        yield "aa\nbb\n"
    producer2 = do
        liftIO $ threadDelay 300000
        yield "cc\ndd\n"
    combinedLines = combined (PT.lines PT.utf8x) (PT.lines PT.utf8x) PT.intoLazyText
    
