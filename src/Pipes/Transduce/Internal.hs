{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Transduce.Internal (
        FoldP(..)
    ,   foldFallibly
    ,   Pipes.Transduce.Internal.fold
    ,   TransducerP(..)
    ,   mapper 
    ,   fallibleMapper 
    ,   mapperFoldable 
    ,   mapperEnumerable 
    ,   transducer
    ,   fallibleTransducer
    ,   delimit
    ,   transduce
    ) where

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
import Control.Concurrent
import Control.Concurrent.Conceit
import Control.Exception
import Pipes 
import Pipes.Lift (distribute) 
import Pipes.Prelude
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import Pipes.Concurrent
import Lens.Family (folding)

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

foldFallibly :: FoldP b e a -> Producer b IO r -> IO (Either e (a,r))
foldFallibly (FoldP (unLift -> s)) = exhaustiveCont s

fold :: FoldP b Void a -> Producer b IO r -> IO (a,r)
fold (FoldP (unLift -> s)) = liftM (either absurd id) . exhaustiveCont s

--premapP :: (a -> b) -> FoldP b e r -> FoldP a e r 
--premapP f (FoldP s) = FoldP $ case s of
--    Pure p -> Pure p
--    Other o -> Other $ case o of
--        TrueFold e -> TrueFold $ Foldl.premapM f e
--        ExhaustiveCont e -> ExhaustiveCont $ \producer ->
--            e $ producer >-> Pipes.map f
--        NonexhaustiveCont ne -> NonexhaustiveCont $ \producer ->
--            ne $ producer >-> Pipes.map f
--
--premapFoldableP :: Foldable f => (a -> f b) -> FoldP b e r -> FoldP a e r 
---- could be more efficient for TrueFold
--premapFoldableP unwinder = premapEnumerableP (Select . each . unwinder)
--
--premapEnumerableP :: Enumerable t => (a -> t IO b) -> FoldP b e r -> FoldP a e r 
--premapEnumerableP unwinder s = 
--    FoldP (Other (ExhaustiveCont (foldP s . flip for (enumerate . toListT . unwinder))))
--

data TransducerP b e a = 
      Mapper (b -> a)
    | Folder (b -> [a])
    | P2P (forall r. Producer b IO r -> Producer a IO r)
    | P2PE (forall r. Producer b IO r -> Producer a IO (Either e r))
    | Splitting (forall r. Producer b IO r -> FreeT (Producer a IO) IO r)
    | SplittingE (forall r. Producer b IO r -> FreeT (Producer a IO) IO (Either e r))

instance Functor (TransducerP b e) where
  fmap = second

instance Bifunctor (TransducerP b) where
  bimap f g s = case s of
      Mapper x -> Mapper (g . x)
      Folder x -> Folder (fmap g . x)
      P2P x -> P2P (\producer -> for (x producer) (Pipes.yield . g))
      P2PE x -> P2PE (\producer -> liftM (first f) (for (x producer) (Pipes.yield . g)))
      Splitting x -> Splitting (\producer -> transFreeT (\p -> for p (Pipes.yield . g)) (x producer))
      SplittingE x -> SplittingE (\producer -> liftM (first f) (transFreeT (\p -> (for p (Pipes.yield . g))) (x producer)))

mapper :: (a -> b) -> TransducerP a e b
mapper = Mapper

fallibleMapper :: (a -> Either e b) -> TransducerP a e b
fallibleMapper fallible = P2PE (\producer -> (runExceptT . distribute) (for (hoist lift producer) (\a -> do
    case fallible a of
        Left e -> lift (throwE e)
        Right b -> Pipes.yield b)))

mapperFoldable :: Foldable f => (a -> f b) -> TransducerP a e b
mapperFoldable f = Folder (Data.Foldable.toList . f)

mapperEnumerable :: Enumerable f => (a -> f IO b) -> TransducerP a e b
mapperEnumerable enumerable = P2P (\producer -> for producer (enumerate . toListT . enumerable))

transducer :: (forall r. Producer b IO r -> Producer a IO r) -> TransducerP b e a
transducer = P2P

fallibleTransducer :: (forall r. Producer b IO r -> Producer a IO (Either e r)) -> TransducerP b e a
fallibleTransducer = P2PE

delimit :: (forall r. Producer a IO r -> FreeT (Producer a' IO) IO r) -> TransducerP b e a -> TransducerP b e a'
delimit f t = case t of
    Mapper func -> Splitting (\producer -> f (producer >-> Pipes.Prelude.map func))
    Folder func -> Splitting (\producer -> f (producer >-> mapFoldable func))
    P2P g -> Splitting (f . g)
    P2PE g -> SplittingE (f . g)
    Splitting g -> Splitting (f . Pipes.concats . g)
    SplittingE g -> SplittingE (f . Pipes.concats . g)

transduce :: TransducerP b e a -> FoldP a e r -> FoldP b e r
transduce (Mapper _) (FoldP (Pure x)) = 
    FoldP (Pure x)
transduce (Mapper f) (FoldP (Other (TrueFold x))) = 
    FoldP (Other (TrueFold (Foldl.premapM f x)))
transduce (Mapper f) (FoldP (Other (ExhaustiveCont x))) = 
    FoldP (Other (ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f))))
transduce (Mapper f) (FoldP (Other (NonexhaustiveCont x))) = 
    FoldP (Other (NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f))))
--
transduce (Folder _) (FoldP (Pure x)) = 
    FoldP (Pure x)
transduce (Folder f) (FoldP (Other (TrueFold x))) = 
    FoldP (Other (TrueFold (Foldl.handlesM (folding f) x)))
transduce (Folder f) (FoldP (Other (ExhaustiveCont x))) = 
    FoldP (Other (ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f))))
transduce (Folder f) (FoldP (Other (NonexhaustiveCont x))) = 
    FoldP (Other (NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f))))
--
transduce (P2P f) (FoldP (unLift -> s)) = case s of
    NonexhaustiveCont x -> FoldP (Other (NonexhaustiveCont (x . f)))
    _ -> FoldP (Other (ExhaustiveCont (exhaustiveCont s . f)))
--
transduce (Splitting f) somefold = transduce (P2P (Pipes.concats . f)) somefold
--
transduce (P2PE f) (FoldP (unLift -> s)) = case s of
    TrueFold aTrueFold -> undefined
    _ -> undefined
--
transduce (SplittingE f) somefold = transduce (P2PE (Pipes.concats . f)) somefold





