{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Foldl.Transduce.Pipes.Internal (
        FoldP(..)
    ,   foldP
    ,   premapP
    ,   premapFoldableP
    ,   premapEnumerableP
    ,   TransducerP(..)
    ,   transducerP
    ,   fallibleTransducerP
    ,   splitP
    ,   transduceP
    ) where

import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free hiding (Pure)
import qualified Control.Foldl as Foldl
import Control.Concurrent
import Control.Concurrent.Conceit
import Control.Exception
import Pipes 
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import Pipes.Concurrent

newtype FoldP b e a = FoldP (Lift (FoldP_ b e) a) deriving (Functor)

data FoldP_ b e a = 
         TrueFold (Foldl.FoldM (ExceptT e IO) b a)
       | ExhaustiveCont (forall r. Producer b IO r -> IO (Either e (a,r)))
       | NonexhaustiveCont (Producer b IO () -> IO (Either e a))
       deriving (Functor)

instance Applicative (FoldP b e) where
    pure a = FoldP (pure a)
    FoldP fa <*> FoldP a = FoldP (fa <*> a)

instance Applicative (FoldP_ b e) where
    pure a = ExhaustiveCont (\producer -> do
        r <- runEffect (producer >-> Pipes.drain)
        pure (Right (a,r)))

    TrueFold f1 <*> TrueFold f2 = TrueFold (f1 <*> f2)
    s1 <*> s2 = bifurcate (nonexhaustiveCont s1) (nonexhaustiveCont s2)  
        where 
        bifurcate fs as = ExhaustiveCont (\producer -> do
            (outbox1,inbox1,seal1) <- spawn' (bounded 1)
            (outbox2,inbox2,seal2) <- spawn' (bounded 1)
            runConceit $
                (\f x r -> (f x,r))
                <$>
                Conceit (fs (fromInput inbox1) `finally` atomically seal1)
                <*>
                Conceit (as (fromInput inbox2) `finally` atomically seal2)
                <*>
                (_Conceit $
                    (runEffect (producer >-> Pipes.tee (toOutput outbox1 *> Pipes.drain) 
                                         >->           (toOutput outbox2 *> Pipes.drain)))
                    `finally` atomically seal1 
                    `finally` atomically seal2))

instance Bifunctor (FoldP_ b) where
  bimap f g s = case s of
      TrueFold (Foldl.FoldM step start done) -> TrueFold (Foldl.FoldM 
          (\previous input -> withExceptT f (step previous input))
          (withExceptT f start)
          (\final -> withExceptT f (fmap g (done final))))
      ExhaustiveCont u -> ExhaustiveCont (fmap (liftM  (bimap f (bimap g id))) u)
      NonexhaustiveCont h -> NonexhaustiveCont (fmap (liftM  (bimap f g)) h)

instance Bifunctor (FoldP b) where
  bimap f g (FoldP s) = FoldP (case s of
      Pure a -> Pure (g a)
      Other o -> Other (bimap f g o))

instance (Monoid a) => Monoid (FoldP b e a) where
   mempty = pure mempty
   mappend s1 s2 = (<>) <$> s1 <*> s2

nonexhaustiveCont :: FoldP_ b e a -> Producer b IO () -> IO (Either e a)
nonexhaustiveCont (TrueFold e) = \producer -> runExceptT (Foldl.impurely Pipes.foldM e (hoist lift producer))
nonexhaustiveCont (ExhaustiveCont e) = \producer -> liftM (fmap fst) (e producer)
nonexhaustiveCont (NonexhaustiveCont u) = u

exhaustiveCont :: FoldP_ b e a -> Producer b IO r -> IO (Either e (a,r))
exhaustiveCont s = case s of 
    TrueFold e -> \producer -> 
        runExceptT (Foldl.impurely Pipes.foldM' e (hoist lift producer))
    ExhaustiveCont e -> e
    NonexhaustiveCont activity -> \producer -> do 
        (outbox,inbox,seal) <- spawn' (bounded 1)
        runConceit $ 
            (,) 
            <$>
            Conceit (activity (fromInput inbox) `finally` atomically seal)
            <*>
            (_Conceit $
                (runEffect (producer >-> (toOutput outbox *> Pipes.drain)) 
                `finally` atomically seal))

------------------------------------------------------------------------------
------------------------------------------------------------------------------

foldP :: FoldP b e a -> Producer b IO r -> IO (Either e (a,r))
foldP (FoldP (unLift -> s)) = exhaustiveCont s

premapP :: (a -> b) -> FoldP b e r -> FoldP a e r 
premapP f (FoldP s) = FoldP $ case s of
    Pure p -> Pure p
    Other o -> Other $ case o of
        TrueFold e -> TrueFold $ Foldl.premapM f e
        ExhaustiveCont e -> ExhaustiveCont $ \producer ->
            e $ producer >-> Pipes.map f
        NonexhaustiveCont ne -> NonexhaustiveCont $ \producer ->
            ne $ producer >-> Pipes.map f

premapFoldableP :: Foldable f => (a -> f b) -> FoldP b e r -> FoldP a e r 
-- could be more efficient for TrueFold
premapFoldableP unwinder = premapEnumerableP (Select . each . unwinder)

premapEnumerableP :: Enumerable t => (a -> t IO b) -> FoldP b e r -> FoldP a e r 
premapEnumerableP unwinder s = 
    FoldP (Other (ExhaustiveCont (foldP s . flip for (enumerate . toListT . unwinder))))

data TransducerP b e a = 
      P2P (forall r. Producer b IO r -> Producer a IO r)
    | P2PE (forall r. Producer b IO r -> Producer a IO (Either e r))
    | Splitting (forall r. Producer b IO r -> FreeT (Producer a IO) IO r)
    | SplittingE (forall r. Producer b IO r -> FreeT (Producer a IO) IO (Either e r))

instance Functor (TransducerP b e) where
  fmap = second

instance Bifunctor (TransducerP b) where
  bimap f g s = case s of
      P2P x -> P2P (\producer -> for (x producer) (Pipes.yield . g))
      P2PE x -> P2PE (\producer -> liftM (first f) (for (x producer) (Pipes.yield . g)))
      Splitting x -> Splitting (\producer -> transFreeT (\p -> for p (Pipes.yield . g)) (x producer))
      SplittingE x -> SplittingE (\producer -> liftM (first f) (transFreeT (\p -> (for p (Pipes.yield . g))) (x producer)))

transducerP :: (forall r. Producer b IO r -> Producer a IO r) -> TransducerP b e a
transducerP = P2P

fallibleTransducerP :: (forall r. Producer b IO r -> Producer a IO (Either e r)) -> TransducerP b e a
fallibleTransducerP = P2PE

splitP :: (forall r. Producer a IO r -> FreeT (Producer a' IO) IO r) -> TransducerP b e a -> TransducerP b e a'
splitP f t = case t of
    P2P g -> Splitting (f . g)
    P2PE g -> SplittingE (f . g)
    Splitting g -> Splitting (f . Pipes.concats . g)
    SplittingE g -> SplittingE (f . Pipes.concats . g)

transduceP :: TransducerP b e a -> FoldP a e r -> FoldP b e r
transduceP f (FoldP (unLift -> s)) = undefined




