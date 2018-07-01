module Main where

import Prelude

import Control.Comonad.Store (Store, StoreT(..))
import Control.Monad.State (State, put)
import Data.Identity (Identity(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Lib (Component, Console(..), UI, explore)

options :: 
  Array 
    { label :: String
    , value :: Int
    }
options = [
  { value: 1, label: "One" },
  { value: 2, label: "Two" },
  { value: 3, label: "Three" }
]

type Props =
  { count :: Int
  , selectedValue :: Int
  }

selectComponent :: Component Effect (Store Props) (State Props) Console
selectComponent = StoreT $ Tuple (Identity render) { count: 1, selectedValue: 0 }
  where
    render :: Props -> UI Effect (State Props) Console
    render { count, selectedValue } send = Console {
      value: show selectedValue,
      onChange: \input -> do
        log $ "You've played " <> show count <> " times."
        case fromString input of
          Nothing -> send do
            log "Please enter an Integer."
            pure $ put { count: count + 1, selectedValue }
          Just x -> send do
            log $ "Selected value: " <> input
            let renderOptions option = log $ option.label <> 
              if option.value == x then " *" else ""
            traverse_ renderOptions options
            pure $ put { count: count + 1, selectedValue: x }
                 
      } 

main :: Effect Unit
main = do
  explore selectComponent