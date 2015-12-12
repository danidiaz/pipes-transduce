{-# LANGUAGE RankNTypes #-}

module Pipes.Transduce.Text (
        -- * Collecting input
        intoLazyText 
        -- * Splitting
    ,   lines
        -- * Grouping
    ,   foldedLines
        -- * Decoding
    ,   decoder
    ,   decoderx
    ,   utf8
    ,   utf8x
    ) where

import Prelude hiding (lines)
import Data.Bifunctor
import Data.Monoid
import Data.Void
import Data.Foldable
import Data.ByteString
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
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import qualified Pipes.Parse
import qualified Pipes.Text
import qualified Data.Text.Lazy
import Pipes.Concurrent
import Lens.Family (view)

import Pipes.Transduce
import Pipes.Transduce.Internal

foldedLines 
    :: Transducer' Continuous Text e Data.Text.Lazy.Text 
foldedLines = 
    Pipes.Transduce.folds 
    (fmap Data.Text.Lazy.fromChunks (Pipes.Transduce.withFold Foldl.list)) 
    (lines (Pipes.Transduce.mapper id))

lines 
    :: Transducer' Continuous a e Text -- ^
    -> Transducer' Delimited a e Text -- ^
lines sometrans = delimit (view Pipes.Text.lines) sometrans

decoder 
    :: (forall r. Producer ByteString IO r -> Producer Text IO (Producer ByteString IO r))
    -> Transducer' Continuous ByteString ByteString Text -- ^
decoder f = PE (\producer -> f producer >>= \producer' -> lift (do
    n <- next producer'
    case n of
        Left r -> return (Right r)
        Right b -> return (Left (fst b))))

decoderx
    :: (forall r. Producer ByteString IO r -> Producer Text IO (Producer ByteString IO r))
    -> Transducer' Continuous ByteString e Text -- ^
decoderx f = P (\producer -> f producer >>= \producer' -> lift (do
    n <- next producer'
    case n of
        Left r -> return r
        Right b -> throwIO (DecodeError "transducer decoding error" (Just (Data.ByteString.head (fst b)))))) 

utf8 :: Transducer' Continuous ByteString ByteString Text -- ^
utf8 = decoder decodeUtf8

utf8x :: Transducer' Continuous ByteString e Text -- ^
utf8x = decoderx decodeUtf8

intoLazyText :: Fold' Text e Data.Text.Lazy.Text
intoLazyText = fmap Data.Text.Lazy.fromChunks (withFold Foldl.list)
