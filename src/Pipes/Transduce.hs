module Pipes.Transduce (
        -- * Producer folds
        Fold1
    ,   foldFallibly1
    ,   Pipes.Transduce.Internal.fold1
        -- * Multiple Producer folds
    ,   Fold2
    ,   foldFallibly2
    ,   fold2
    ,   separated
    ,   combined
        -- * Wat
    ,   Fold2I
    ,   foldFallibly2I
    ,   fold2I
    ,   promote
        -- * Building folds
        -- ** From foldl folds
        -- $foldl
    ,   withFold 
    ,   withFoldIO 
    ,   withFallibleFold
        -- ** From consumers
        -- $consumers
    ,   withConsumer 
    ,   withConsumer' 
    ,   withConsumerM 
    ,   withConsumerM' 
    ,   withSafeConsumer 
    ,   withFallibleConsumer 
        -- ** From parsers
        -- $parsers
    ,   withParser 
    ,   withParserM 
        -- ** From continuations
        -- $continuations
    ,   withCont 
    ,   withCont' 
    ,   withFallibleCont 
    ,   withFallibleCont'  
        -- * Transducers
    ,   Transducer
    ,   Delimited
    ,   Continuous
    ,   transduce1
        -- * Building transducers
    ,   mapper 
    ,   fallibleMapper 
    ,   mapperFoldable 
    ,   mapperEnumerable 
    ,   transducer
    ,   fallibleTransducer
        -- * Transducer group operations
    ,   delimit
    ,   groups
    ,   folds
    ,   concats
    ,   intercalates
        -- * Utilities
    ,   trip
    ,   tripx
    ) where

import Pipes.Transduce.Internal 

import Data.Bifunctor
import Data.Monoid
import Data.Void
import Data.Foldable
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free hiding (Pure)
import qualified Control.Foldl as Foldl
import Control.Exception
import Pipes 
import Pipes.Lift (distribute) 
--import Pipes.Prelude
import qualified Pipes.Prelude as Pipes

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
    Fail if the 'Producer' produces anything at all. The error value is what came
    out of the 'Producer'.

>>> PT.foldFallibly1 trip (mapM_ yield ['z']) 
Left 'z'

>>> PT.foldFallibly1 trip (mapM_ yield []) 
Right ((),())
-}
trip :: Fold1 b b ()
trip = withFallibleCont' $ \producer -> do
    n <- next producer  
    return $ case n of 
        Left r -> Right ((),r)
        Right (b,_) -> Left b

{-| 
    Throw an exception if the 'Producer' produces anything at all

    __/BEWARE!/__ 
    This 'Transducer may throw 'AssertionFailed'.
    __/BEWARE!/__ 

>>> PT.foldFallibly1 tripx (mapM_ yield ['z']) 
*** Exception: tripx
-}
tripx :: Fold1 b e ()
tripx = withFallibleCont' $ \producer -> do
    n <- next producer  
    case n of 
        Left r -> return (Right ((),r))
        Right _ -> throwIO (AssertionFailed "tripx")


{- $foldl
 
    'Fold1' values can be created out of the more general folds of the @foldl@
    library, which are producer-agnostic.
-} 

{- $consumers
 
    'Fold1' values can be created out of 'Consumer's from the @pipes@ library.
-}

{- $parsers
 
    'Fold1' values can be created out of 'Parser's from the @pipes-parse@ library.
-}

{- $continuations
 
    The most general way of constructing 'Fold1' values is from an arbitrary
    function that consumes a 'Producer'.
-}
