module Pipes.Transduce (
        -- * Producer folds
        FoldP
    ,   foldFallibly
    ,   Pipes.Transduce.Internal.fold
        -- * Building folds
        -- ** From continuations
    ,   withFallibleCont 
    ,   withFallibleCont'  
    ,   withCont 
    ,   withCont' 
        -- ** From foldl Folds
    ,   withFold 
    ,   withFoldIO 
    ,   withFallibleFoldIO 
    ,   withFoldM 
        -- ** From Consumers
    ,   withConsumer 
    ,   withConsumer' 
    ,   withConsumerM 
    ,   withConsumerM' 
    ,   withSafeConsumer 
    ,   withFallibleConsumer 
        -- ** From Parsers
    ,   withParser 
    ,   withParserM 
        -- * Transducers
    ,   TransducerP
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
    ) where

import Pipes.Transduce.Internal 




