module App.Board where

import Data.Maybe (Maybe)

type Board =
  { id :: String
  , name :: String
  , board :: Array (Maybe Int)
  }
