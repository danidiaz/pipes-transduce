module Main where

import Prelude hiding (splitAt,lines,words)
import Data.Char
import Data.String hiding (lines,words)
import Data.Monoid
import Data.Bifunctor
import qualified Data.List (intersperse,splitAt)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Control.Foldl as L

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    []
