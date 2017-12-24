{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Transduce.Text (
        -- * Text folds
        intoLazyText 
    ,   asUtf8
    ,   asUtf8x
    ,   bothAsUtf8x
    ,   Line
    ,   asFoldedLines
    ,   eachLine
    ,   combinedLines
    ,   combinedLinesPrefixing
        -- * Text transducers
        -- ** Decoding
    ,   decoder
    ,   decoderx
    ,   utf8
    ,   utf8x
        -- ** Splitting
    ,   lines
    ,   lines_
    ,   foldedLines
    ) where

import Prelude hiding (lines)
import Data.Bifunctor
import Data.ByteString
import qualified Data.Text 
import qualified Data.Text.Lazy
import Data.Text hiding (lines)
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Control.Foldl as Foldl
import Control.Exception
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Pipes 
import qualified Pipes.Text
import Pipes.Text.Encoding (decodeUtf8) 

import           Pipes.Transduce.Internal 

{- $setup
>>> :set -XOverloadedStrings
>>> import qualified Data.Text as T 
>>> import qualified Data.Text.Lazy as TL 
>>> import Control.Applicative
>>> import Control.Monad
>>> import qualified Control.Foldl as L
>>> import Pipes.Transduce.Internal
>>> import Pipes.Transduce.Text
-}

{-| 
    Whole lines are represented as lazy 'Data.Text.Lazy.Text' values.
-}
type Line = Data.Text.Lazy.Text 

{-| 
    Split the stream into lines, collect them into lazy 'Text' values, and pass
    them downstream. 

>>> fold1 (transduce1 foldedLines (withFold L.list)) (mapM_ yield ["aa","aa\nbb","bb"]) 
(["aaaa","bbbb"],())

-}
foldedLines 
    :: Transducer Continuous Text e Line
foldedLines = 
    folds 
    (fmap Data.Text.Lazy.fromChunks (withFold Foldl.list)) 
    (lines_ (mapper id))

{-| 

Transforms a 'Fold1' that accepts whole lines into a 'Fold1' that accepts and
undivided text stream.

>>> fold1 (asFoldedLines (withFold L.list)) (mapM_ yield ["aa","aa\nbb","bb"]) 
(["aaaa","bbbb"],())

-}
asFoldedLines :: Fold1 Line e r -> Fold1 Text e r
asFoldedLines = transduce1 foldedLines 

{-| 
    Split the stream into lines, collect them into lazy 'Text' values, and
    apply an effectul function to each line.

>>> fold1Fallibly (eachLine $ \l -> pure $ if TL.head l == 'b' then (Left l) else (Right ())) (mapM_ yield ["aa","\nbb"]) 
Left "bb"

-}
eachLine :: (Line -> IO (Either e ())) -> Fold1 Data.Text.Text e ()
eachLine action = transduce1 foldedLines (withFallibleConsumer (forever (do
    await >>= lift . ExceptT . action)))

{-| 
    Process two streams of text, combined as a single text stream.

    The streams are combined line by line, but the resulting stream is undivided.

>>> fold2 (combinedLines intoLazyText) (mapM_ yield ["aa"]) (mapM_ yield ["aa"])
("aa\naa\n",(),())

-}
combinedLines :: Fold1 Text e r -> Fold2 Text Text e r
combinedLines = 
    combined (Pipes.Transduce.Text.lines (transducer id)) 
             (Pipes.Transduce.Text.lines (transducer id))

{-| 
    Like 'combinedLines', but adding different prefixes to lines from stdout
    and stderr.

>>> fold2 (combinedLinesPrefixing "-" "-" intoLazyText) (mapM_ yield ["aa"]) (mapM_ yield ["aa"])
("-aa\n-aa\n",(),())

-}
combinedLinesPrefixing :: Text -> Text -> Fold1 Text e r -> Fold2 Text Text e r
combinedLinesPrefixing outprefix errprefix = 
    let tag prefix = groups (\producer -> Pipes.yield prefix *> producer)
    in
    combined (tag outprefix (Pipes.Transduce.Text.lines (transducer id))) 
             (tag errprefix (Pipes.Transduce.Text.lines (transducer id)))

{-| 
    Split into lines, eliding newlines.

>>> fold1 (transduce1 (concats . groups (\p -> yield "x" *> p) . lines_ $ utf8x) intoLazyText) (mapM_ yield ["aa\n","bb"]) 
("xaaxbb",())

-}
lines_ 
    :: Transducer Continuous a e Text -- ^
    -> Transducer Delimited a e Text -- ^
lines_ sometrans = delimit (view Pipes.Text.lines) sometrans

{-| 
    Split into lines, preserving newlines.

>>> fold1 (transduce1 (concats . groups (\p -> yield "x" *> p) . lines $ utf8x) intoLazyText) (mapM_ yield ["aa\n","bb"]) 
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

decoderx'
    :: (forall r. Producer ByteString IO r -> Producer Text IO (Producer ByteString IO r))
    -> Producer ByteString IO x -> Producer Text IO x 
decoderx' f = (\producer -> f producer >>= \producer' -> lift (do
    n <- next producer'
    case n of
        Left r -> return r
        Right b -> throwIO (DecodeError "transducer decoding error" (Just (Data.ByteString.head (fst b)))))) 

{-| 
    The first undecodable bytes will be the error value.

>>> fold1Fallibly (transduce1 utf8 intoLazyText) (mapM_ yield ["aa"]) 
Right ("aa",())

-}
utf8 :: Transducer Continuous ByteString ByteString Text -- ^
utf8 = decoder decodeUtf8

{-| 

>>> fold1  (transduce1 utf8x intoLazyText) (mapM_ yield ["aa"]) 
("aa",())

    __/BEWARE!/__ 
    This 'Transducer' may throw 'DecodeError'.
    __/BEWARE!/__ 
-}
utf8x :: Transducer Continuous ByteString e Text -- ^
utf8x = decoderx decodeUtf8

{-| 

Turns a fold that accepts `Text` into a fold that accepts UTF8-encoded
`ByteString`.

It also takes a function that maps undecoded leftovers to a more general error
type. 

>>> fold1Fallibly (asUtf8 id intoLazyText) (mapM_ yield ["aa"]) 
Right ("aa",())
 
 -}
asUtf8 :: (ByteString -> e) -> Fold1 Text e r -> Fold1 ByteString e r
asUtf8 erradapt = transduce1 (first erradapt utf8)

{-| 

Like 'asUtf8', but throws exceptions in case of decoding errors.

>>> fold1  (asUtf8x intoLazyText) (mapM_ yield ["aa"]) 
("aa",())
 
    __/BEWARE!/__ 
    This 'Transducer' may throw 'DecodeError'.
    __/BEWARE!/__ 
 -}
asUtf8x :: Fold1 Text e r -> Fold1 ByteString e r
asUtf8x = transduce1 utf8x

{-| 
>>> fold2 (bothAsUtf8x (combinedLines intoLazyText)) (mapM_ yield ["aa"]) (mapM_ yield ["aa"])
("aa\naa\n",(),())

    __/BEWARE!/__ 
    This 'Transducer' may throw 'DecodeError'.
    __/BEWARE!/__ 
 -}
bothAsUtf8x :: Fold2 Text Text e r -> Fold2 ByteString ByteString e r
bothAsUtf8x (Fold2 (unLift -> f)) = case f of
    First f1  -> Fold2 (Other (First $ unwrap (trans f1)))
    Second f1 -> Fold2 (Other (Second $ unwrap (trans f1)))
    Both both -> Fold2 (Other (Both $ \f1 f2 -> both (dec f1) (dec f2)))
    where 
    dec = decoderx' decodeUtf8
    trans = transduce1 (transducer dec) . Fold1 . Other
    unwrap (Fold1 z) = unLift z

{-| 
    Collect strict 'Text's into a lazy 'Text'.

>>> fold1  intoLazyText (mapM_ yield ["aa","bb","cc"]) 
("aabbcc",())

-}
intoLazyText :: Fold1 Text e Data.Text.Lazy.Text
intoLazyText = fmap Data.Text.Lazy.fromChunks (withFold Foldl.list)

-- Lens stuff
type Getting r s a = (a -> Const r a) -> s -> Const r s

view :: Getting a s a -> s -> a
view l s = getConst (l Const s)
{-# INLINE view #-}

