module LensHelpers
( (.-)
, withLens
, module Control.Lens
, module Control.Monad
, module Control.Monad.Trans
, module Control.Monad.Trans.State
) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

(.-) g fcn = g %= execState fcn

withLens lens program = lens.-program
