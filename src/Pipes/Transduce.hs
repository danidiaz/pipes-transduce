module Pipes.Transduce (
        -- * Producer folds
        Fold'
    ,   foldFallibly
    ,   Pipes.Transduce.Internal.fold
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
    ,   Transducer'
    ,   transduce
    ,   Delimited
    ,   Continuous
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

>>> PT.foldFallibly trip (mapM_ yield ['z']) 
Left 'z'

>>> PT.foldFallibly trip (mapM_ yield []) 
Right ((),())
-}
trip :: Fold' b b ()
trip = withFallibleCont' $ \producer -> do
    n <- next producer  
    return $ case n of 
        Left r -> Right ((),r)
        Right (b,_) -> Left b

{-| 
    Throw an exception if the 'Producer' produces anything at all

    __/BEWARE!/__ 
    This 'Transducer' may throw 'AssertionFailed'.
    __/BEWARE!/__ 

>>> PT.foldFallibly tripx (mapM_ yield ['z']) 
*** Exception: tripx
-}
tripx :: Fold' b e ()
tripx = withFallibleCont' $ \producer -> do
    n <- next producer  
    case n of 
        Left r -> return (Right ((),r))
        Right _ -> throwIO (AssertionFailed "tripx")


{- $foldl
 
    'Fold'' values can be created out of the more general folds of the @foldl@
    library, which are producer-agnostic.
-} 

{- $consumers
 
    'Fold'' values can be created out of 'Consumer's from the @pipes@ library.
-}

{- $parsers
 
    'Fold'' values can be created out of 'Parser's from the @pipes-parse@ library.
-}

{- $continuations
 
    The most general way of constructing 'Fold'' values is from an arbitrary
    function that consumes a 'Producer'.
-}
