{-# LANGUAGE RankNTypes #-}

module Pipes.Transduce.ByteString (
        intoLazyBytes
    ,   driveHandle
    ,   driveHandleFallibly
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
import Pipes.Prelude
import qualified Pipes.ByteString (hGetSome)
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import qualified Pipes.Parse
import qualified Pipes.Text
import qualified Data.Text.Lazy
import Pipes.Concurrent
import Lens.Family (view)
import System.IO

import Pipes.Transduce

intoLazyBytes :: Fold' ByteString e Data.ByteString.Lazy.ByteString
intoLazyBytes = fmap Data.ByteString.Lazy.fromChunks (withFold Foldl.list)

driveHandleFallibly 
    :: Fold' ByteString e r 
    -> Int -- ^ max chunk size
    -> Handle 
    -> IO (Either e r) 
driveHandleFallibly somefold chunkSize handle =
    fmap (bimap id fst) (Pipes.Transduce.foldFallibly somefold (Pipes.ByteString.hGetSome chunkSize handle))

driveHandle
    :: Fold' ByteString Void r 
    -> Int -- ^ max chunk size
    -> Handle 
    -> IO r
driveHandle somefold chunkSize handle =
    fmap fst (Pipes.Transduce.fold somefold (Pipes.ByteString.hGetSome chunkSize handle))

