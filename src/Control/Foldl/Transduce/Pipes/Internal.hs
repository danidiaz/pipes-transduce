{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Foldl.Transduce.Pipes.Internal (
        FoldP
    ) where

import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import qualified Control.Foldl as Foldl
import Control.Concurrent
import Control.Concurrent.Conceit
import Control.Exception
import Pipes 
import qualified Pipes.Prelude as Pipes
import Pipes.Concurrent

newtype FoldP b e a = FoldP (Lift (FoldP_ b e) a) deriving (Functor)

data FoldP_ b e a = 
         Exhaustive (forall r. Producer b IO r -> IO (Either e (a,r)))
       | Nonexhaustive (Producer b IO () -> IO (Either e a))
       deriving (Functor)

instance Applicative (FoldP b e) where
    pure a = FoldP (pure a)
    (FoldP fa) <*> (FoldP a) = FoldP (fa <*> a)

instance Applicative (FoldP_ b e) where
    pure a = Exhaustive (\producer -> do
        r <- runEffect (producer >-> Pipes.drain)
        pure (Right (a,r)))

    s1 <*> s2 = bifurcate (nonexhaustive s1) (nonexhaustive s2)  
        where 
        bifurcate fs as = Exhaustive (\producer -> do
            (outbox1,inbox1,seal1) <- spawn' (bounded 1)
            (outbox2,inbox2,seal2) <- spawn' (bounded 1)
            runConceit (liftA2 (,)
                (Conceit (fs (fromInput inbox1) `finally` atomically seal1) 
                 <*>
                 Conceit (as (fromInput inbox2) `finally` atomically seal2))
                (_Conceit (runEffect (do 
                      producer >-> Pipes.tee (toOutput outbox1 >> Pipes.drain) 
                               >->           (toOutput outbox2 >> Pipes.drain))   
                      `finally` atomically seal1 `finally` atomically seal2))))


nonexhaustive :: FoldP_ b e a -> Producer b IO () -> IO (Either e a)
nonexhaustive (Exhaustive e) = \producer -> liftM (fmap fst) (e producer)
nonexhaustive (Nonexhaustive u) = u
