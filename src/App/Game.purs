-- | The Game manager
-- |
-- | This module exposes types to represent the actual game and functions to perform
-- | checks for winning conditions
module App.Game
  ( winConditions
  , checkWinCondition
  , Direction(..)
  , Slant(..)
  , WinCondition(..)
  ) where

import Prelude

import App.Board (Board, Letter(..), MarkedValue, getIndexInBoard)
import Data.Array (all, index)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst)

-- | The direction for a line winning condition
-- |
-- | - `Horizontal` - for horizontal lines checking
-- | - `Vertical` - for vertical lines checking
data Direction
  = Horizontal
  | Vertical

derive instance genericDirection :: Generic Direction _
derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

instance Show Direction where
  show = genericShow

data Slant
  = Forward
  | Backward

derive instance genericSlant :: Generic Slant _
derive instance eqSlant :: Eq Slant
derive instance ordSlant :: Ord Slant

instance Show Slant where
  show = genericShow

data WinCondition
  = Line Direction
  | Diagonal Slant

derive instance genericWinCondition :: Generic WinCondition _
derive instance eqWinCondition :: Eq WinCondition
derive instance ordWinCondition :: Ord WinCondition

instance Show WinCondition where
  show = genericShow

-- | The list of winning conditions
winConditions :: Array WinCondition
winConditions =
  [ Line Horizontal
  , Line Vertical
  , Diagonal Forward
  , Diagonal Backward
  ]

check :: Int -> Array MarkedValue -> Boolean
check idx values = fromMaybe false $ fst <$> index values idx

-- | Given the last value assigned check if it fulfilled the active winning condition
-- INFO: Abstract, board, size, dynamic, etc, timing. Blah.
checkWinCondition :: Letter -> Int -> WinCondition -> Board -> Boolean
checkWinCondition letter num (Line Horizontal) board = fromMaybe false $ do
  lastIndex <- getIndexInBoard letter num board
  let values = L.toUnfoldable $ M.values board.board
  pure $ all (check lastIndex) values
checkWinCondition letter _ (Line Vertical) board = fromMaybe false $ do
  column <- M.lookup letter board.board
  pure $ all fst column
checkWinCondition _ _ (Diagonal Forward) { board } = fromMaybe false $ do
  Tuple b _ <- flip index 4 =<< M.lookup B board
  Tuple i _ <- flip index 3 =<< M.lookup I board
  Tuple g _ <- flip index 1 =<< M.lookup G board
  Tuple o _ <- flip index 0 =<< M.lookup O board
  pure $ b && i && g && o
checkWinCondition _ _ (Diagonal Backward) { board } = fromMaybe false $ do
  Tuple b _ <- flip index 0 =<< M.lookup B board
  Tuple i _ <- flip index 1 =<< M.lookup I board
  Tuple g _ <- flip index 3 =<< M.lookup G board
  Tuple o _ <- flip index 4 =<< M.lookup O board
  pure $ b && i && g && o
