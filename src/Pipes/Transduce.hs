module Pipes.Transduce (
        -- * Producer folds
        FoldP
    ,   foldFallibly
    ,   Pipes.Transduce.Internal.fold
        -- * Building folds
        -- ** From continuations
    ,   fromFallibleCont 
    ,   fromFallibleCont'  
    ,   fromCont 
    ,   fromCont' 
        -- ** From foldl Folds
    ,   fromFold 
    ,   fromFoldIO 
    ,   fromFallibleFoldIO 
    ,   fromFoldM 
        -- ** From Consumers
    ,   fromConsumer 
    ,   fromConsumer' 
    ,   fromConsumerM 
    ,   fromConsumerM' 
    ,   fromSafeConsumer 
    ,   fromFallibleConsumer 
        -- ** From Parsers
    ,   fromParser 
    ,   fromParserM 
        -- * Transducers
    ,   TransducerP
    ,   transduce
        -- * Building transducers
    ,   mapper 
    ,   fallibleMapper 
    ,   mapperFoldable 
    ,   mapperEnumerable 
    ,   transducer
    ,   fallibleTransducer
        -- * Transducer group operations
    ,   delimit
    ) where

import Pipes.Transduce.Internal 




