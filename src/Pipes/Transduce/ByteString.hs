{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pipes.Transduce.ByteString (
        -- * Collecting  input
        intoLazyBytes
        -- * Reading from handles
    ,   drainHandle
    ,   drainHandleFallibly
    ,   ChunkSize
    ,   chunkSize
    ,   chunkSizeDefault
    ) where

import Prelude hiding (lines)
import Data.Bifunctor
import Data.Void
import Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Control.Foldl as Foldl
import qualified Pipes.ByteString (hGetSome)
import System.IO
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import Pipes.Transduce

{- $setup
>>> :set -XOverloadedStrings
>>> import Control.Applicative
>>> import Control.Monad
>>> import Pipes
>>> import qualified Pipes.Transduce as PT
-}

{-| 
    Collect strict 'ByteString's into a lazy 'ByteString'.

>>> PT.fold1  intoLazyBytes (mapM_ yield ["aa","bb","cc"]) 
("aabbcc",())

-}
intoLazyBytes :: Fold1 ByteString e Data.ByteString.Lazy.ByteString
intoLazyBytes = fmap Data.ByteString.Lazy.fromChunks (withFold Foldl.list)

drainHandleFallibly 
    :: Fold1 ByteString e r 
    -> ChunkSize 
    -> Handle 
    -> IO (Either e r) 
drainHandleFallibly somefold (ChunkSize csize) handle =
    fmap (bimap id fst) (Pipes.Transduce.fold1Fallibly somefold (Pipes.ByteString.hGetSome csize handle))

drainHandle
    :: Fold1 ByteString Void r 
    -> ChunkSize  
    -> Handle 
    -> IO r
drainHandle somefold (ChunkSize csize) handle =
    fmap fst (Pipes.Transduce.fold1 somefold (Pipes.ByteString.hGetSome csize handle))

{-| Maximum chunk size      
-}
newtype ChunkSize = ChunkSize Int deriving (Show,Eq,Ord,Num)

chunkSize :: Int -> ChunkSize
chunkSize = ChunkSize

chunkSizeDefault :: ChunkSize
chunkSizeDefault = chunkSize defaultChunkSize
