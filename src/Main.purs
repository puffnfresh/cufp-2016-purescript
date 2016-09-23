module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

main :: forall e. Eff (channel :: CHANNEL, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
