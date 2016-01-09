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

{-| 
    A computation in 'IO' that completely drains a 'Producer' of @b@ values,
    returning a value of type @a@, except when it fails early with an error of
    type @e@.
-}
newtype Fold1 b e a = Fold1 (Lift (Fold1_ b e) a) deriving (Functor)

data Fold1_ b e a = 
         TrueFold (Foldl.FoldM (ExceptT e IO) b a)
       | ExhaustiveCont (forall r. Producer b IO r -> IO (Either e (a,r)))
       | NonexhaustiveCont (Producer b IO () -> IO (Either e a))
       deriving (Functor)

{-| 
    'pure' creates a 'Fold1' that does nothing besides draining the
    'Producer'. 

    '<*>' feeds both folds with the data of the same 'Producer'. If any of
    them fails the combination fails.
-}
instance Applicative (Fold1 b e) where
    pure a = Fold1 (pure a)
    Fold1 fa <*> Fold1 a = Fold1 (fa <*> a)

instance Applicative (Fold1_ b e) where
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

instance Bifunctor (Fold1_ b) where
  bimap f g s = case s of
      TrueFold (Foldl.FoldM step start done) -> TrueFold (Foldl.FoldM 
          (\previous input -> withExceptT f (step previous input))
          (withExceptT f start)
          (\final -> withExceptT f (fmap g (done final))))
      ExhaustiveCont u -> ExhaustiveCont (fmap (liftM  (bimap f (bimap g id))) u)
      NonexhaustiveCont h -> NonexhaustiveCont (fmap (liftM  (bimap f g)) h)

{-| 
    'first' is useful to massage errors.
-}
instance Bifunctor (Fold1 b) where
  bimap f g (Fold1 s) = Fold1 (case s of
      Pure a -> Pure (g a)
      Other o -> Other (bimap f g o))

instance (Monoid a) => Monoid (Fold1 b e a) where
   mempty = pure mempty
   mappend s1 s2 = (<>) <$> s1 <*> s2

nonexhaustiveCont :: Fold1_ b e a -> Producer b IO () -> IO (Either e a)
nonexhaustiveCont (TrueFold e) = \producer -> runExceptT (Foldl.impurely Pipes.foldM e (hoist lift producer))
nonexhaustiveCont (ExhaustiveCont e) = \producer -> liftM (fmap fst) (e producer)
nonexhaustiveCont (NonexhaustiveCont u) = u

exhaustiveCont :: Fold1_ b e a -> Producer b IO r -> IO (Either e (a,r))
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
    -> Fold1 b e a 
withFallibleCont f = Fold1 (Other (NonexhaustiveCont f))

withFallibleCont'  
    :: (forall r. Producer b IO r -> IO (Either e (a,r))) -- ^
    -> Fold1 b e a 
withFallibleCont' f = Fold1 (Other (ExhaustiveCont f))

withCont 
    :: (Producer b IO () -> IO a) -- ^
    -> Fold1 b e a -- ^
withCont aFold = withFallibleCont $ fmap (fmap pure) $ aFold

withCont' 
    :: (forall r. Producer b IO r -> IO (a,r)) -- ^
    -> Fold1 b e a -- ^
withCont' aFold = withFallibleCont' $ fmap (fmap pure) aFold

withFold :: Foldl.Fold b a -> Fold1 b e a 
withFold aFold = Fold1 (Other (TrueFold (Foldl.generalize aFold)))

withFoldIO :: Foldl.FoldM IO b a -> Fold1 b e a 
withFoldIO aFold = Fold1 (Other (TrueFold (hoistFold lift aFold)))

hoistFold :: Monad m => (forall a. m a -> n a) -> Foldl.FoldM m i r -> Foldl.FoldM n i r 
hoistFold g (Foldl.FoldM step begin done) = Foldl.FoldM (\s i -> g (step s i)) (g begin) (g . done)

withFallibleFold :: Foldl.FoldM (ExceptT e IO) b a -> Fold1 b e a 
withFallibleFold aFold = Fold1 (Other (TrueFold aFold))

--withFoldM 
--    :: MonadIO m 
--    => (forall r. m (a,r) -> IO (Either e (c,r))) 
--    -> Foldl.FoldM m b a 
--    -> Fold1 b e c 
--withFoldM whittle aFoldM = withFallibleCont' $ \producer -> 
--    whittle $ Foldl.impurely Pipes.Prelude.foldM' aFoldM (hoist liftIO producer)

withConsumer :: Consumer b IO () -> Fold1 b e ()
withConsumer consumer = withCont $ \producer -> runEffect $ producer >-> consumer 

{-| Builds a 'Fold1' out of a 'Consumer' that never stops by itself.

-}
withConsumer' :: Consumer b IO Void -> Fold1 b e ()
withConsumer' consumer = withCont' $ \producer -> fmap ((,) ()) $ runEffect $ producer >-> fmap absurd consumer 

withConsumerM :: MonadIO m 
              => (m () -> IO (Either e a))  -- ^
              -> Consumer b m () 
              -> Fold1 b e a
withConsumerM whittle consumer = withFallibleCont $ \producer -> whittle $ runEffect $ (hoist liftIO producer) >-> consumer 

withConsumerM' :: MonadIO m 
               => (forall r. m r -> IO (Either e (a,r))) -- ^
               -> Consumer b m Void
               -> Fold1 b e a
withConsumerM' whittle consumer = withFallibleCont' $ \producer -> whittle $ runEffect $ (hoist liftIO producer) >-> fmap absurd consumer 

withSafeConsumer 
    :: Consumer b (SafeT IO) Void -- ^
    -> Fold1 b e ()
withSafeConsumer = withConsumerM' (fmap (\r -> Right ((),r)) . runSafeT)

withFallibleConsumer 
    :: Consumer b (ExceptT e IO) Void -- ^
    -> Fold1 b e ()
withFallibleConsumer = withConsumerM' (fmap (fmap (\r -> ((), r))) . runExceptT)


withParser 
    :: Pipes.Parse.Parser b IO (Either e a) -- ^
    -> Fold1 b e a 
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
            -> Pipes.Parse.Parser b m a -> Fold1 b e c 
withParserM f parser = withFallibleCont' $ \producer -> f $ drainage $ (Pipes.Parse.runStateT parser) (hoist liftIO producer)
  where
    drainage m = do 
        (a,leftovers) <- m
        r <- runEffect (leftovers >-> Pipes.Prelude.drain)
        return (a,r)

------------------------------------------------------------------------------

{-| 
    Run a 'Fold1'.
-}
foldFallibly1 :: Fold1 b e a -> Producer b IO r -> IO (Either e (a,r))
foldFallibly1 (Fold1 (unLift -> s)) = exhaustiveCont s

{-| 
    Run a 'Fold1' that never returns an error value (but which may still throw exceptions!)
-}
fold1 :: Fold1 b Void a -> Producer b IO r -> IO (a,r)
fold1 (Fold1 (unLift -> s)) = liftM (either absurd id) . exhaustiveCont s

{-| A transformation that takes the inputs of a 'Fold1' from type @a@ to type @b@.		

    Optionally, the transformation may delimit groups of elements in the
    stream. In that case the phantom type @x@ will be 'Delimited'. Otherwise, it will be
    'Continuous'.
-}
data Transducer x b e a = 
      M (b -> a)
    | F (b -> [a])
    | P (forall r. Producer b IO r -> Producer a IO r)
    | PE (forall r. Producer b IO r -> Producer a IO (Either e r))
    | S (forall r. Producer b IO r -> FreeT (Producer a IO) IO r)
    | SE (forall r. Producer b IO r -> FreeT (Producer a IO) IO (Either e r))

instance Functor (Transducer x b e) where
  fmap = second

instance Bifunctor (Transducer x b) where
  bimap f g s = case s of
      M x -> M (g . x)
      F x -> F (fmap g . x)
      P x -> P (\producer -> for (x producer) (Pipes.yield . g))
      PE x -> PE (\producer -> liftM (first f) (for (x producer) (Pipes.yield . g)))
      S x -> S (\producer -> transFreeT (\p -> for p (Pipes.yield . g)) (x producer))
      SE x -> SE (\producer -> liftM (first f) (transFreeT (\p -> (for p (Pipes.yield . g))) (x producer)))

mapper 
    :: (a -> b) -- ^
    -> Transducer Continuous a e b
mapper = M

fallibleM 
    :: (a -> Either e b) -- ^
    -> Transducer Continuous a e b  -- ^
fallibleM fallible = PE (\producer -> (runExceptT . distribute) (for (hoist lift producer) (\a -> do
    case fallible a of
        Left e -> lift (throwE e)
        Right b -> Pipes.yield b)))

fallibleMapper 
    :: (a -> Either e b) -- ^
    -> Transducer Continuous a e b  -- ^
fallibleMapper fallible = PE (\producer -> (runExceptT . distribute) (for (hoist lift producer) (\a -> do
    case fallible a of
        Left e -> lift (throwE e)
        Right b -> Pipes.yield b)))

mapperFoldable 
    :: Foldable f 
    => (a -> f b) -- ^
    -> Transducer Continuous a e b -- ^
mapperFoldable f = F (Data.Foldable.toList . f)

mapperEnumerable 
    :: Enumerable f 
    => (a -> f IO b) -- ^
    -> Transducer Continuous a e b  -- ^
mapperEnumerable enumerable = P (\producer -> for producer (enumerate . toListT . enumerable))

transducer 
    :: (forall r. Producer b IO r -> Producer a IO r)  -- ^
    -> Transducer Continuous b e a -- ^
transducer = P

fallibleTransducer 
    :: (forall r. Producer b IO r -> Producer a IO (Either e r))  -- ^
    -> Transducer Continuous b e a  -- ^
fallibleTransducer = PE

{-| Plug splitting functions from @pipes-group@ here.		

-}
delimit 
    :: (forall r. Producer a IO r -> FreeT (Producer a' IO) IO r) -- ^
    -> Transducer Continuous b e a -- ^
    -> Transducer Delimited b e a' -- ^
delimit f t = case t of
    M func -> S (\producer -> f (producer >-> Pipes.Prelude.map func))
    F func -> S (\producer -> f (producer >-> mapFoldable func))
    P g -> S (f . g)
    PE g -> SE (f . g)
    S g -> S (f . Pipes.concats . g)
    SE g -> SE (f . Pipes.concats . g)

{-| Apply a 'Transducer' to a 'Fold1'.		

-}
transduce1 :: Transducer Continuous b e a -> Fold1 a e r -> Fold1 b e r
transduce1 (M _) (Fold1 (Pure x)) = 
    Fold1 (Pure x)
transduce1 (M f) (Fold1 (Other s)) = (Fold1 (Other (case s of
    TrueFold x -> TrueFold (Foldl.premapM f x)
    ExhaustiveCont x -> ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f))
    NonexhaustiveCont x -> NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.map f)))))
transduce1 (F _) (Fold1 (Pure x)) = 
    Fold1 (Pure x)
transduce1 (F f) (Fold1 (Other s)) = (Fold1 (Other (case s of
    TrueFold x -> TrueFold (Foldl.handlesM (folding f) x)
    ExhaustiveCont x -> ExhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f))
    NonexhaustiveCont x -> NonexhaustiveCont (\producer -> x (producer >-> Pipes.Prelude.mapFoldable f)))))
transduce1 (P f) (Fold1 (unLift -> s)) = case s of
    NonexhaustiveCont x -> Fold1 (Other (NonexhaustiveCont (x . f)))
    _ -> Fold1 (Other (ExhaustiveCont (exhaustiveCont s . f)))
transduce1 (PE f) (Fold1 (exhaustiveCont . unLift -> s)) = do
    Fold1 (Other (ExhaustiveCont (\producer -> do
        (outbox,inbox,seal) <- spawn' (bounded 1)
        runConceit $ 
            (\(r,()) r' -> (r,r'))
            <$>
            Conceit (s (fromInput inbox) `finally` atomically seal)
            <*>
            (Conceit $
                (runEffect (f producer >-> (toOutput outbox *> Pipes.drain)) 
                `finally` atomically seal)))))
transduce1 (S f) somefold = transduce1 (P (Pipes.concats . f)) somefold
transduce1 (SE f) somefold = transduce1 (PE (Pipes.concats . f)) somefold

{-| Tweak each of the groups delimited by a 'Transducer'.		

-}
groups 
    :: (forall r. Producer b IO r -> Producer b' IO r) -- ^
    -> Transducer Delimited a e b  -- ^
    -> Transducer Delimited a e b' -- ^
groups f t = case t of
    M func -> P (f . (\producer -> producer >-> Pipes.Prelude.map func))
    F func -> P (f . (\producer -> producer >-> mapFoldable func))
    P g -> P (f . g)
    PE g -> PE (f . g)
    S g -> S (Pipes.maps f . g)
    SE g -> SE (Pipes.maps f . g)

folds 
    :: Fold1 b Void b' -- ^
    -> Transducer Delimited a e b 
    -> Transducer Continuous a e b'
folds somefold t = case t of
    M func -> folds somefold (P (\producer -> producer >-> Pipes.Prelude.map func))
    F func -> folds somefold (P (\producer -> producer >-> mapFoldable func))
    P g -> folds somefold (S (liftF . g))
    PE g -> folds somefold (SE (liftF . g))
    S g -> P (Pipes.concats . transFreeT ((\action -> lift action >>= (\(b',r) -> Pipes.yield b' >> return r)) . Pipes.Transduce.Internal.fold1 somefold) . g)
    SE g -> PE (Pipes.concats . transFreeT ((\action -> lift action >>= (\(b',r) -> Pipes.yield b' >> return r)) . Pipes.Transduce.Internal.fold1 somefold) . g)

data Delimited

data Continuous

concats 
    :: Transducer Delimited a e b   -- ^
    -> Transducer Continuous a e b
concats t =  case t of
    M func -> M func
    F func -> F func
    P g -> P g
    PE g -> PE g
    S g -> P (Pipes.concats . g)
    SE g -> PE (Pipes.concats . g)

intercalates 
    :: Producer b IO ()  -- ^
    -> Transducer Delimited a e b 
    -> Transducer Continuous a e b
intercalates p t =  case t of
    M func -> M func
    F func -> F func
    P g -> P g
    PE g -> PE g
    S g -> P (Pipes.intercalates p . g)
    SE g -> PE (Pipes.intercalates p . g)



newtype Fold2 b1 b2 e a = Fold2 (forall r1 r2. Producer b1 IO r1 -> Producer b2 IO r2 -> IO (Either e (a,r1,r2))) deriving (Functor)

instance Bifunctor (Fold2 b1 b2) where
    bimap f g (Fold2 s) = Fold2 (fmap (fmap (fmap (bimap f (\(x1,x2,x3) -> (g x1,x2,x3))))) s) 

instance Applicative (Fold2 b1 b2 e) where
    pure a = Fold2 (\producer1 producer2 -> do
        (r1,r2) <- _runConceit $
            (,)
            <$>
            _Conceit (runEffect (producer1 >-> Pipes.drain))
            <*>
            _Conceit (runEffect (producer2 >-> Pipes.drain))
        pure (Right (a,r1,r2)))

    Fold2 fs <*> Fold2 as = Fold2 (\producer1 producer2 -> do
        (outbox1a,inbox1a,seal1a) <- spawn' (bounded 1)
        (outbox2a,inbox2a,seal2a) <- spawn' (bounded 1)
        (outbox1b,inbox1b,seal1b) <- spawn' (bounded 1)
        (outbox2b,inbox2b,seal2b) <- spawn' (bounded 1)
        runConceit $
            (\(f,(),()) (x,(),()) r1 r2 -> (f x,r1,r2))
            <$>
            Conceit (fs (fromInput inbox1a) (fromInput inbox1b) `finally` atomically seal1a `finally` atomically seal1b)
            <*>
            Conceit (as (fromInput inbox2a) (fromInput inbox2b) `finally` atomically seal2a `finally` atomically seal2b)
            <*>
            (_Conceit $
                (runEffect (producer1 >-> Pipes.tee (toOutput outbox1a *> Pipes.drain) 
                                      >->           (toOutput outbox2a *> Pipes.drain)))
                `finally` atomically seal1a 
                `finally` atomically seal2a)
            <*>
            (_Conceit $
                (runEffect (producer2 >-> Pipes.tee (toOutput outbox1b *> Pipes.drain) 
                                      >->           (toOutput outbox2b *> Pipes.drain)))
                `finally` atomically seal1b 
                `finally` atomically seal2b))

{-| 
    Run a 'Fold2'.
-}
foldFallibly2 :: Fold2 b1 b2 e a -> Producer b1 IO r1 -> Producer b2 IO r2 -> IO (Either e (a,r1,r2))
foldFallibly2 (Fold2 s) producer1 producer2 = s producer1 producer2


{-| 
    Run a 'Fold2'.
-}
fold2 :: Fold2 b1 b2 Void a -> Producer b1 IO r1 -> Producer b2 IO r2 -> IO (a,r1,r2)
fold2 (Fold2 s) producer1 producer2 = liftM (either absurd id) (s producer1 producer2) 

separated :: Fold1 b1 e r1 -> Fold1 b2 e r2 -> Fold2 b1 b2 e (r1,r2)
separated f1 f2 = Fold2 (\producer1 producer2 ->
    runConceit $
        (\(r1,x1) (r2,x2) -> ((r1,r2),x1,x2))
        <$>
        Conceit (foldFallibly1 f1 producer1)
        <*>
        Conceit (foldFallibly1 f2 producer2))

combined :: Transducer Delimited b1 e x -> Transducer Delimited b2 e x -> Fold1 x e a -> Fold2 b1 b2 e a
combined t1 t2 f = Fold2 (\producer1 producer2 -> do
   (outbox, inbox, seal) <- spawn' (bounded 1)
   lock <- newMVar outbox
   runConceit $ 
       (\((),r1) ((),r2) (a,()) -> (a,r1,r2))
       <$>
       Conceit (foldFallibly1 (transduce1 (folds (withCont' (iterTLines lock)) t1) (pure ())) producer1 `finally` atomically seal)
       <*>
       Conceit (foldFallibly1 (transduce1 (folds (withCont' (iterTLines lock)) t2) (pure ())) producer2 `finally` atomically seal)
       <*>
       Conceit (foldFallibly1 f (fromInput inbox) `finally` atomically seal))
  where
    -- iterTLines mvar = iterT $ \textProducer -> do
    iterTLines mvar = \textProducer -> fmap (\x -> ((),x)) $ do
        -- the P.drain bit was difficult to figure out!!!
        withMVar mvar $ \output -> do
            runEffect $ textProducer >-> (toOutput output >> Pipes.drain)
