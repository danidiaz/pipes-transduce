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
import Data.Monoid
import Data.Void
import Data.Foldable
import Data.ByteString
import qualified Data.ByteString.Lazy
import Data.Text hiding (lines)
import Data.Text.Encoding.Error (UnicodeException(..))
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free hiding (Pure)
import qualified Control.Foldl as Foldl
import Control.Exception
import Pipes 
import qualified Pipes.Text
import Pipes.Text.Encoding (decodeUtf8) 
import Pipes.Lift (distribute) 
--import Pipes.Prelude
import qualified Pipes.ByteString (hGetSome)
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import qualified Pipes.Parse
import qualified Pipes.Text
import qualified Data.Text.Lazy
import Pipes.Concurrent
import Lens.Family (view)
import System.IO
import Data.ByteString.Lazy.Internal (defaultChunkSize)

import Pipes.Transduce

{- $setup
>>> :set -XOverloadedStrings
>>> import Control.Applicative
>>> import Control.Monad
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
    fmap (bimap id fst) (Pipes.Transduce.foldFallibly1 somefold (Pipes.ByteString.hGetSome csize handle))

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
