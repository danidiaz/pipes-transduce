module Pipes.Transduce (
        -- * Producer folds
        Fold'
    ,   foldFallibly
    ,   Pipes.Transduce.Internal.fold
        -- * Building folds
        -- ** From foldl folds
    ,   withFold 
    ,   withFoldIO 
    ,   withFallibleFold
        -- ** From consumers
    ,   withConsumer 
    ,   withConsumer' 
    ,   withConsumerM 
    ,   withConsumerM' 
    ,   withSafeConsumer 
    ,   withFallibleConsumer 
        -- ** From parsers
    ,   withParser 
    ,   withParserM 
        -- ** From continuations
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




