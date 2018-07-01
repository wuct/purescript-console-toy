module Lib 
  ( Console(..)
  , Component
  , UI
  , explore
  ) where

import Prelude

import Control.Comonad (class Comonad, duplicate, extract)
import Control.Comonad.Store (Store, runStore)
import Control.Monad.State (State, runState)
import Data.Functor.Pairing (type (⋈))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (new, read, write)
import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

move :: forall w m a b. Comonad w => Monad m => m ⋈ w -> m a -> w b -> w b
move pairing movement space = pairing (\_ newspace -> newspace) movement (duplicate space)

type UI base m a = (m Unit -> base Unit) -> a
type Component base w m a = w (UI base m a)

stateStore :: forall s. State s ⋈ Store s
stateStore f state store =
  let Tuple sb s = runStore store
      Tuple a s' = runState state s
  in f a (sb s')

data Console = Console {
  value :: String,
  onChange :: String -> Effect Unit
}

explore :: forall s. Component Effect (Store s) (State s) Console -> Effect Unit
explore component = do
  componentRef <- new component
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 0 interface
  prompt interface
  setLineHandler interface $ \s ->
    if s == "q"
      then do
        log "Bye bye."
        close interface
      else do 
        space <- read componentRef
        let send action = write (move stateStore action space) componentRef
        let Console { onChange } = extract space send
        onChange s
        prompt interface 