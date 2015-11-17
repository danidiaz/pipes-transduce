module Pipes.Transduce.Text (lines) where

import Prelude hiding (lines)
import Data.Bifunctor
import Data.Monoid
import Data.Void
import Data.Foldable
import Data.Text hiding (lines)
import Control.Applicative
import Control.Applicative.Lift
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free hiding (Pure)
import qualified Control.Foldl as Foldl
import Control.Exception
import Pipes 
import qualified Pipes.Text
import Pipes.Lift (distribute) 
import Pipes.Prelude
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Group as Pipes
import qualified Pipes.Parse
import Pipes.Concurrent
import Lens.Family (view)

import Pipes.Transduce

lines 
    :: Transducer' Continuous a e Text -- ^
    -> Transducer' Delimited a e Text -- ^
lines sometrans = delimit (view Pipes.Text.lines) sometrans

