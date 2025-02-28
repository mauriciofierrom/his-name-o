-- | A Bingo Board
-- |
-- | This module exposes types to represent a Bingo Board and functions to
-- | perform operations in them
module App.Board
  ( Id
  , Index
  , Board
  , MarkedValue
  , BoardValue(..)
  , parseBoardValue
  , toString
  , emptyBoard
  , Letter(..)
  , updateValue
  , updateMark
  ) where

import Prelude

import Data.Array (findIndex, modifyAt, replicate, zip)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (drop, toUpper)
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..))

-- | Id of a board
-- |
-- | The pk of the table in IndexedDb
type Id = Int

-- | Index of a value of a column
type Index = Int

type MarkedValue = Tuple Boolean BoardValue

-- | The representation of a Bingo Board
-- |
-- | `id` - The pk for this board. It's maybe to allow to represent non-persisted boards
-- |
-- | `name` - The name for the board. It's OK for a board to be an empty string
-- |
-- | `board` - The grid itself is represented as a map from a `Letter` to a column of `BoardValue`s
type Board =
  { id :: Maybe Id
  , name :: String
  , board :: Map Letter (Array MarkedValue)
  }

-- | The representation of a value in a board
-- |
-- | `Number` - represents an value in a column
-- |
-- | `Empty` represents the absence of values in a column
-- |
-- | `None` represents a value in a column that must not allow inserting/showing values.
-- | Typically represents the center box in a 5x5 grid.
data BoardValue
  = Number Int
  | Empty
  | None

derive instance genericBoardValue :: Generic BoardValue _
derive instance eqBoardValue :: Eq BoardValue

instance Show BoardValue where
  show = genericShow

-- | Parses a board value from its textual representation e.g. as a value from an `input` element
-- |
-- | Note that we don't cover the `None` case because we don't need to use its value ever
parseBoardValue :: String -> BoardValue
parseBoardValue "" = Empty
parseBoardValue value = maybe Empty Number $ fromString value

-- | Convert a Board value to a textual representation e.g. as a value for an `input` element
-- | or to display it
toString :: BoardValue -> String
toString (Number number) = show number
toString _ = ""

-- | An example of an empty board
emptyBoard :: Board
emptyBoard = { id: Nothing, name: "", board: initialValues }

-- | The letters of the Bingo Card. Also his-name-o
-- |
-- | Used as keys in the map tha represents the board grid
data Letter
  = B
  | I
  | N
  | G
  | O

derive instance genericLetter :: Generic Letter _
derive instance eqLetter :: Eq Letter
derive instance ordLetter :: Ord Letter

instance Show Letter where
  show = genericShow

instance Enum Letter where
  pred = genericPred
  succ = genericSucc

instance Bounded Letter where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum Letter where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

parseLetter :: Char -> Maybe Letter
parseLetter 'B' = Just B
parseLetter 'I' = Just I
parseLetter 'N' = Just N
parseLetter 'G' = Just G
parseLetter 'O' = Just O
parseLetter _ = Nothing

allLetters :: Array Letter
allLetters = enumFromTo bottom top

empties :: Letter -> Array BoardValue
empties N = [ Empty, Empty, None, Empty, Empty ]
empties _ = replicate 5 Empty

initialValues :: Map Letter (Array MarkedValue)
initialValues =
  let
    initialMark letter = if letter == N then [ false, false, true, false, false ] else replicate 5 false
  in
    M.fromFoldable $ map (\letter -> Tuple letter (zip (initialMark letter) $ empties letter)) allLetters

updateColumnValue :: Index -> String -> Array MarkedValue -> Array MarkedValue
updateColumnValue idx value values =
  fromMaybe values $ modifyAt idx (\(Tuple marked _value) -> Tuple marked $ parseBoardValue value) values

-- | Update a value in a provided column at the provided index
updateValue :: Letter -> Index -> String -> Board -> Board
updateValue letter idx value leBoard =
  leBoard { board = M.update (Just <<< updateColumnValue idx value) letter leBoard.board }

-- Unmark a value in the board
updateMark :: String -> Board -> Board
updateMark rawNumber board = fromMaybe board $ do
  Tuple letter number <- parseNumber rawNumber
  column <- M.lookup letter board.board
  idx <- findIndex (numberPred number) column
  pure $ markValue letter idx board
  where
  numberPred number (Tuple _ (Number n)) = n == number
  numberPred _ _ = false

markValue :: Letter -> Index -> Board -> Board
markValue letter idx board =
  board { board = M.update (Just <<< updateMarkAtIndex idx) letter board.board }

updateMarkAtIndex :: Index -> Array MarkedValue -> Array MarkedValue
updateMarkAtIndex idx values =
  fromMaybe values $ modifyAt idx (\(Tuple mark value) -> Tuple (not mark) value) values

parseNumber :: String -> Maybe (Tuple Letter Int)
parseNumber str = do
  firstChar <- charAt 0 $ toUpper str
  letter <- parseLetter firstChar
  let numberStr = drop 1 str
  number <- fromString numberStr
  pure $ Tuple letter number
