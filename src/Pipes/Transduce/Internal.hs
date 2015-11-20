{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}

module Pipes.Transduce.Internal where

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
import qualified Pipes.Parse
import Pipes.Concurrent
import Pipes.Safe (SafeT, runSafeT)
import Lens.Family (folding)

newtype Fold' b e a = Fold' (Lift (Fold'_ b e) a) deriving (Functor)

data Fold'_ b e a = 
         TrueFold (Foldl.FoldM (ExceptT e IO) b a)
       | ExhaustiveCont (forall r. Producer b IO r -> IO (Either e (a,r)))
       | NonexhaustiveCont (Producer b IO () -> IO (Either e a))
       deriving (Functor)

instance Applicative (Fold' b e) where
    pure a = Fold' (pure a)
    Fold' fa <*> Fold' a = Fold' (fa <*> a)

instance Applicative (Fold'_ b e) where
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

instance Bifunctor (Fold'_ b) where
  bimap f g s = case s of
      TrueFold (Foldl.FoldM step start done) -> TrueFold (Foldl.FoldM 
          (\previous input -> withExceptT f (step previous input))
          (withExceptT f start)
          (\final -> withExceptT f (fmap g (done final))))
      ExhaustiveCont u -> ExhaustiveCont (fmap (liftM  (bimap f (bimap g id))) u)
      NonexhaustiveCont h -> NonexhaustiveCont (fmap (liftM  (bimap f g)) h)

instance Bifunctor (Fold' b) where
  bimap f g (Fold' s) = Fold' (case s of
      Pure a -> Pure (g a)
      Other o -> Other (bimap f g o))

instance (Monoid a) => Monoid (Fold' b e a) where
   mempty = pure mempty
   mappend s1 s2 = (<>) <$> s1 <*> s2

nonexhaustiveCont :: Fold'_ b e a -> Producer b IO () -> IO (Either e a)
nonexhaustiveCont (TrueFold e) = \producer -> runExceptT (Foldl.impurely Pipes.foldM e (hoist lift producer))
nonexhaustiveCont (ExhaustiveCont e) = \producer -> liftM (fmap fst) (e producer)
nonexhaustiveCont (NonexhaustiveCont u) = u

exhaustiveCont :: Fold'_ b e a -> Producer b IO r -> IO (Either e (a,r))
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


withFallibleCont 
    :: (Producer b IO () -> IO (Either e a)) -- ^
    -> Fold' b e a 
withFallibleCont f = Fold' (Other (NonexhaustiveCont f))

withFallibleCont'  
    :: (forall r. Producer b IO r -> IO (Either e (a,r))) -- ^
    -> Fold' b e a 
withFallibleCont' f = Fold' (Other (ExhaustiveCont f))

withCont 
    :: (Producer b IO () -> IO a) -- ^
    -> Fold' b e a -- ^
withCont aFold = withFallibleCont $ fmap (fmap pure) $ aFold

withCont' 
    :: (forall r. Producer b IO r -> IO (a,r)) -- ^
    -> Fold' b e a -- ^
withCont' aFold = withFallibleCont' $ fmap (fmap pure) aFold

withFold :: Foldl.Fold b a -> Fold' b e a 
withFold aFold = Fold' (Other (TrueFold (Foldl.generalize aFold)))

withFoldIO :: Foldl.FoldM IO b a -> Fold' b e a 
withFoldIO aFold = Fold' (Other (TrueFold (hoistFold lift aFold)))

hoistFold :: Monad m => (forall a. m a -> n a) -> Foldl.FoldM m i r -> Foldl.FoldM n i r 
hoistFold g (Foldl.FoldM step begin done) = Foldl.FoldM (\s i -> g (step s i)) (g begin) (g . done)

withFallibleFold :: Foldl.FoldM (ExceptT e IO) b a -> Fold' b e a 
withFallibleFold aFold = Fold' (Other (TrueFold aFold))

--withFoldM 
--    :: MonadIO m 
--    => (forall r. m (a,r) -> IO (Either e (c,r))) 
--    -> Foldl.FoldM m b a 
--    -> Fold' b e c 
--withFoldM whittle aFoldM = withFallibleCont' $ \producer -> 
--    whittle $ Foldl.impurely Pipes.Prelude.foldM' aFoldM (hoist liftIO producer)

withConsumer :: Consumer b IO () -> Fold' b e ()
withConsumer consumer = withCont $ \producer -> runEffect $ producer >-> consumer 

withConsumer' :: Consumer b IO Void -> Fold' b e ()
withConsumer' consumer = withCont' $ \producer -> fmap ((,) ()) $ runEffect $ producer >-> fmap absurd consumer 

withConsumerM :: MonadIO m 
              => (m () -> IO (Either e a))  -- ^
              -> Consumer b m () 
              -> Fold' b e a
withConsumerM whittle consumer = withFallibleCont $ \producer -> whittle $ runEffect $ (hoist liftIO producer) >-> consumer 

withConsumerM' :: MonadIO m 
               => (forall r. m r -> IO (Either e (a,r))) -- ^
               -> Consumer b m Void
               -> Fold' b e a
withConsumerM' whittle consumer = withFallibleCont' $ \producer -> whittle $ runEffect $ (hoist liftIO producer) >-> fmap absurd consumer 

withSafeConsumer 
    :: Consumer b (SafeT IO) Void -- ^
    -> Fold' b e ()
withSafeConsumer = withConsumerM' (fmap (\r -> Right ((),r)) . runSafeT)

withFallibleConsumer 
    :: Consumer b (ExceptT e IO) Void -- ^
    -> Fold' b e ()
withFallibleConsumer = withConsumerM' (fmap (fmap (\r -> ((), r))) . runExceptT)


withParser 
    :: Pipes.Parse.Parser b IO (Either e a) -- ^
    -> Fold' b e a 
withParser parser = withFallibleCont' $ \producer -> drainage $ Pipes.Parse.runStateT parser producer
  where
    drainage m = do 
        (a,leftovers) <- m
        r <- runEffect (leftovers >-> Pipes.Prelude.drain)
        case a of
            Left e -> return (Left e)
            Right a' -> return (Right (a',r)) 

withParserM :: MonadIO m 
            => (forall r. m (a,r) -> IO (Either e (c,r))) -- ^
            -> Pipes.Parse.Parser b m a -> Fold' b e c 
withParserM f parser = withFallibleCont' $ \producer -> f $ drainage $ (Pipes.Parse.runStateT parser) (hoist liftIO producer)
  where
    drainage m = do 
        (a,leftovers) <- m
        r <- runEffect (leftovers >-> Pipes.Prelude.drain)
        return (a,r)

------------------------------------------------------------------------------

foldFallibly :: Fold' b e a -> Producer b IO r -> IO (Either e (a,r))
foldFallibly (Fold' (unLift -> s)) = exhaustiveCont s

fold :: Fold' b Void a -> Producer b IO r -> IO (a,r)
fold (Fold' (unLift -> s)) = liftM (either absurd id) . exhaustiveCont s

data Transducer' x b e a = 
      M (b -> a)
    | F (b -> [a])
    | P (forall r. Producer b IO r -> Producer a IO r)
    | PE (forall r. Producer b IO r -> Producer a IO (Either e r))
    | S (forall r. Producer b IO r -> FreeT (Producer a IO) IO r)
    | SE (forall r. Producer b IO r -> FreeT (Producer a IO) IO (Either e r))

instance Functor (Transducer' x b e) where
  fmap = second

instance Bifunctor (Transducer' x b) where
  bimap f g s = case s of
      M x -> M (g . x)
      F x -> F (fmap g . x)
      P x -> P (\producer -> for (x producer) (Pipes.yield . g))
      PE x -> PE (\producer -> liftM (first f) (for (x producer) (Pipes.yield . g)))
      S x -> S (\producer -> transFreeT (\p -> for p (Pipes.yield . g)) (x producer))
      SE x -> SE (\producer -> liftM (first f) (transFreeT (\p -> (for p (Pipes.yield . g))) (x producer)))

mapper 
    :: (a -> b) -- ^
    -> Transducer' Continuous a e b
mapper = M

fallibleM 
    :: (a -> Either e b) -- ^
    -> Transducer' Continuous a e b  -- ^
fallibleM fallible = PE (\producer -> (runExceptT . distribute) (for (hoist lift producer) (\a -> do
    case fallible a of
        Left e -> lift (throwE e)
        Right b -> Pipes.yield b)))

fallibleMapper 
    :: (a -> Either e b) -- ^
    -> Transducer' Continuous a e b  -- ^
fallibleMapper fallible = PE (\producer -> (runExceptT . distribute) (for (hoist lift producer) (\a -> do
    case fallible a of
        Left e -> lift (throwE e)
        Right b -> Pipes.yield b)))

mapperFoldable 
    :: Foldable f 
    => (a -> f b) -- ^
    -> Transducer' Continuous a e b -- ^
mapperFoldable f = F (Data.Foldable.toList . f)

mapperEnumerable 
    :: Enumerable f 
    => (a -> f IO b) -- ^
    -> Transducer' Continuous a e b  -- ^
mapperEnumerable enumerable = P (\producer -> for producer (enumerate . toListT . enumerable))

transducer 
    :: (forall r. Producer b IO r -> Producer a IO r)  -- ^
    -> Transducer' Continuous b e a -- ^
transducer = P

fallibleTransducer 
    :: (forall r. Producer b IO r -> Producer a IO (Either e r))  -- ^
    -> Transducer' Continuous b e a  -- ^
fallibleTransducer = PE

delimit 
    :: (forall r. Producer a IO r -> FreeT (Producer a' IO) IO r) -- ^
    -> Transducer' Continuous b e a -- ^
    -> Transducer' Delimited b e a' -- ^
delimit f t = case t of
    M func -> S (\producer -> f (producer >-> Pipes.Prelude.map func))
    F func -> S (\producer -> f (producer >-> mapFoldable func))
    P g -> S (f . g)
    PE g -> SE (f . g)
    S g -> S (f . Pipes.concats . g)
    SE g -> SE (f . Pipes.concats . g)

transduce :: Transducer' Continuous b e a -> Fold' a e r -> Fold' b e r
transduce (M _) (Fold' (Pure x)) = 
    Fold' (Pure x)
transduce (M f) (Fold' (Other s)) = (Fold' (Other (case s of
    TrueFold x -> TrueFold (Foldl.premapM f x)
    ExhaustiveCont x -> ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f))
    NonexhaustiveCont x -> NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f)))))
transduce (F _) (Fold' (Pure x)) = 
    Fold' (Pure x)
transduce (F f) (Fold' (Other s)) = (Fold' (Other (case s of
    TrueFold x -> TrueFold (Foldl.handlesM (folding f) x)
    ExhaustiveCont x -> ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f))
    NonexhaustiveCont x -> NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f)))))
transduce (P f) (Fold' (unLift -> s)) = case s of
    NonexhaustiveCont x -> Fold' (Other (NonexhaustiveCont (x . f)))
    _ -> Fold' (Other (ExhaustiveCont (exhaustiveCont s . f)))
transduce (PE f) (Fold' (exhaustiveCont . unLift -> s)) = do
    Fold' (Other (ExhaustiveCont (\producer -> do
        (outbox,inbox,seal) <- spawn' (bounded 1)
        runConceit $ 
            (\(r,()) r' -> (r,r'))
            <$>
            Conceit (s (fromInput inbox) `finally` atomically seal)
            <*>
            (Conceit $
                (runEffect (f producer >-> (toOutput outbox *> Pipes.drain)) 
                `finally` atomically seal)))))
transduce (S f) somefold = transduce (P (Pipes.concats . f)) somefold
transduce (SE f) somefold = transduce (PE (Pipes.concats . f)) somefold

groups 
    :: (forall r. Producer b IO r -> Producer b' IO r) -- ^
    -> Transducer' Delimited a e b  -- ^
    -> Transducer' Delimited a e b' -- ^
groups f t = case t of
    M func -> P (f . (\producer -> producer >-> Pipes.Prelude.map func))
    F func -> P (f . (\producer -> producer >-> mapFoldable func))
    P g -> P (f . g)
    PE g -> PE (f . g)
    S g -> S (Pipes.maps f . g)
    SE g -> SE (Pipes.maps f . g)

folds 
    :: Fold' b Void b' -- ^
    -> Transducer' Delimited a e b 
    -> Transducer' Continuous a e b'
folds somefold t = case t of
    M func -> folds somefold (P (\producer -> producer >-> Pipes.Prelude.map func))
    F func -> folds somefold (P (\producer -> producer >-> mapFoldable func))
    P g -> folds somefold (S (liftF . g))
    PE g -> folds somefold (SE (liftF . g))
    S g -> P (Pipes.concats . transFreeT ((\action -> lift action >>= (\(b',r) -> Pipes.yield b' >> return r)) . Pipes.Transduce.Internal.fold somefold) . g)
    SE g -> PE (Pipes.concats . transFreeT ((\action -> lift action >>= (\(b',r) -> Pipes.yield b' >> return r)) . Pipes.Transduce.Internal.fold somefold) . g)

data Delimited

data Continuous

trip :: Fold' b b ()
trip = withFallibleCont' $ \producer -> do
    n <- next producer  
    return $ case n of 
        Left r -> Right ((),r)
        Right (b,_) -> Left b

tripx :: Fold' b e ()
tripx = withFallibleCont' $ \producer -> do
    n <- next producer  
    case n of 
        Left r -> return (Right ((),r))
        Right _ -> throwIO (AssertionFailed "tripx")
