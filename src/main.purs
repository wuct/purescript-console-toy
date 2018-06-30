module Main where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

type Option a = {
  label :: String,
  value :: a
}

type Props a = {
  value :: a,
  options :: Array (Option a)
}

options :: Array (Option Int)
options = [
  { value: 1, label: "One" },
  { value: 2, label: "Two" },
  { value: 3, label: "Three" }
]

render :: forall a
   . Show a
  => Eq a 
  => Props a -> Effect Unit

render props = do
  log $ "Selected value: " <> show props.value
  traverse_ renderOptions props.options
    where
      renderOptions option = log $ option.label <> if option.value == props.value
        then " *"
        else ""

ui :: Ref Int -> Maybe Int -> Effect Unit
ui _ Nothing = log "Please enter an Integer."
ui countRef (Just selectedValue) = do
  count <- read countRef
  log $ "You've played " <> show count <> " times."
  write (count + 1) countRef
  render { 
    options,
    value: selectedValue 
  }

main :: Effect Unit
main = do
  countRef <- new 0
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 0 interface
  prompt interface
  setLineHandler interface $ \s ->
    if s == "q"
      then close interface
      else do 
        ui countRef $ fromString s      
        prompt interface
  