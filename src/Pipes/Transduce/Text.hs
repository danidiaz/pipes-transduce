{-# LANGUAGE RankNTypes #-}

module Pipes.Transduce.Text (
        -- * Collecting input
        intoLazyText 
        -- * Splitting
    ,   lines
    ,   lines_
        -- * Grouping
    ,   foldedLines
        -- * Decoding
    ,   decoder
    ,   decoderx
    ,   utf8
    ,   utf8x
    ) where

import Prelude hiding (lines)
import Data.ByteString
import qualified Data.Text 
import Data.Text hiding (lines)
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Control.Foldl as Foldl
import Control.Exception
import Pipes 
import qualified Pipes.Text
import Pipes.Text.Encoding (decodeUtf8) 
import qualified Data.Text.Lazy
import Lens.Family (view)

import Pipes.Transduce

{- $setup
>>> :set -XOverloadedStrings
>>> import qualified Data.Text as T 
>>> import qualified Data.Text.Lazy as TL 
>>> import Control.Applicative
>>> import Control.Monad
>>> import qualified Control.Foldl as L
>>> import Pipes.Transduce 
>>> import qualified Pipes.Transduce as PT
>>> import Pipes.Transduce.Text
>>> import qualified Pipes.Transduce.Text as PTT
-}

{-| 
    Split the stream into lines, collect them into lazy 'Text' values, and pass
    them downstream. 

>>> PT.fold1 (transduce1 foldedLines (withFold L.list)) (mapM_ yield ["aa","aa\nbb","bb"]) 
(["aaaa","bbbb"],())

-}
foldedLines 
    :: Transducer Continuous Text e Data.Text.Lazy.Text 
foldedLines = 
    Pipes.Transduce.folds 
    (fmap Data.Text.Lazy.fromChunks (Pipes.Transduce.withFold Foldl.list)) 
    (lines_ (Pipes.Transduce.mapper id))

{-| 
    Split into lines, eliding newlines.

>>> PT.fold1 (transduce1 (concats . groups (\p -> yield "x" *> p) . lines_ $ utf8x) intoLazyText) (mapM_ yield ["aa\n","bb"]) 
("xaaxbb",())

-}
lines_ 
    :: Transducer Continuous a e Text -- ^
    -> Transducer Delimited a e Text -- ^
lines_ sometrans = delimit (view Pipes.Text.lines) sometrans

{-| 
    Split into lines, preserving newlines.

>>> PT.fold1 (transduce1 (concats . groups (\p -> yield "x" *> p) . lines $ utf8x) intoLazyText) (mapM_ yield ["aa\n","bb"]) 
("xaa\nxbb\n",())

-}
lines 
    :: Transducer Continuous a e Text -- ^
    -> Transducer Delimited a e Text -- ^
lines  = groups (\p -> p <* Pipes.yield (Data.Text.singleton '\n')) . lines_

{-| Plug decoding functions from @pipes-text@ here. 

    The first undecodable bytes will be the error value.
-}
decoder 
    :: (forall r. Producer ByteString IO r -> Producer Text IO (Producer ByteString IO r))
    -> Transducer Continuous ByteString ByteString Text -- ^
decoder f = fallibleTransducer (\producer -> f producer >>= \producer' -> lift (do
    n <- next producer'
    case n of
        Left r -> return (Right r)
        Right b -> return (Left (fst b))))

{-| Plug decoding functions from @pipes-text@ here. 

    __/BEWARE!/__ 
    This 'Transducer' may throw 'DecodeError'.
    __/BEWARE!/__ 
-}
decoderx
    :: (forall r. Producer ByteString IO r -> Producer Text IO (Producer ByteString IO r))
    -> Transducer Continuous ByteString e Text -- ^
decoderx f = transducer (\producer -> f producer >>= \producer' -> lift (do
    n <- next producer'
    case n of
        Left r -> return r
        Right b -> throwIO (DecodeError "transducer decoding error" (Just (Data.ByteString.head (fst b)))))) 

{-| 
    The first undecodable bytes will be the error value.

>>> PT.fold1Fallibly (transduce1 utf8 intoLazyText) (mapM_ yield ["aa"]) 
Right ("aa",())

-}
utf8 :: Transducer Continuous ByteString ByteString Text -- ^
utf8 = decoder decodeUtf8

{-| 

>>> PT.fold1  (transduce1 utf8x intoLazyText) (mapM_ yield ["aa"]) 
("aa",())

    __/BEWARE!/__ 
    This 'Transducer' may throw 'DecodeError'.
    __/BEWARE!/__ 
-}
utf8x :: Transducer Continuous ByteString e Text -- ^
utf8x = decoderx decodeUtf8

{-| 
    Collect strict 'Text's into a lazy 'Text'.

>>> PT.fold1  intoLazyText (mapM_ yield ["aa","bb","cc"]) 
("aabbcc",())

-}
intoLazyText :: Fold1 Text e Data.Text.Lazy.Text
intoLazyText = fmap Data.Text.Lazy.fromChunks (withFold Foldl.list)

