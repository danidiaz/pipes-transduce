{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}

module Pipes.Transduce (
        -- * Producer folds
        Fold1
    ,   fold1
    ,   fold1Fallibly
        -- ** Building producer folds
        -- *** From foldl folds
        -- $foldl
    ,   withFold 
    ,   withFoldIO 
    ,   withFallibleFold
        -- *** From consumers
        -- $consumers
    ,   withConsumer 
    ,   withConsumer' 
    ,   withConsumerM 
    ,   withConsumerM' 
    ,   withSafeConsumer 
    ,   withFallibleConsumer 
        -- *** From parsers
        -- $parsers
    ,   withParser 
    ,   withParserM 
        -- *** From continuations
        -- $continuations
    ,   withCont 
    ,   withCont' 
    ,   withFallibleCont 
    ,   withFallibleCont'  
        -- * Fold transducers
    ,   Transducer
    ,   Delimited
    ,   Continuous
    ,   transduce1
        -- ** Building fold transducers
    ,   mapper 
    ,   fallibleMapper 
    ,   mapperFoldable 
    ,   mapperEnumerable 
    ,   transducer
    ,   fallibleTransducer
        -- ** Transducer group operations
    ,   delimit
    ,   groups
    ,   folds
    ,   concats
    ,   intercalates
        -- * Multiple producer folds
    ,   Fold2
    ,   fold2
    ,   fold2Fallibly
        -- ** Building multiple producer folds
    ,   liftFirst
    ,   liftSecond
    ,   separated
    ,   combined
        -- * Utilities
    ,   trip
    ,   tripx
    ) where

import Control.Exception
import Pipes 
import Pipes.Transduce.Internal

{- $setup
>>> :set -XOverloadedStrings
>>> import qualified Data.Text as T 
>>> import qualified Data.Text.Lazy as TL 
>>> import           Control.Applicative
>>> import           Control.Monad
>>> import qualified Control.Foldl as L
>>> import           Pipes.Transduce
>>> import qualified Pipes.Transduce as PT
-}

{-| 
    Fail if the 'Producer' produces anything at all. The error value is what came
    out of the 'Producer'.

>>> PT.fold1Fallibly trip (mapM_ yield ['z']) 
Left 'z'

>>> PT.fold1Fallibly trip (mapM_ yield []) 
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

>>> PT.fold1Fallibly PT.tripx (mapM_ yield ['z']) 
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

