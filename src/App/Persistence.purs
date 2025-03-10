-- | The persistence layer
-- |
-- | This module contains the persistence logic to store and retreive boards to/from an `IndexedDB`
-- | database via FFI. It defines an auto-marshalled representation of the board and functions to convert
-- | between persisted and non-persisted versions of the boards.
module App.Persistence
  ( toPersistentBoard
  , fromPersistentBoard
  , openDB
  , loadBoards
  , saveBoard
  , deleteBoard
  -- | These are required due for the FFI functions
  , PersistedBoard
  , Values
  ) where

import Prelude

import Data.Array (zip)
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as M
import Effect (Effect)
import Control.Promise (Promise)
import Effect.Uncurried (EffectFn1)
import App.Board (Board, BoardValue(..), Letter(..))

type Values =
  { b :: Array Int
  , i :: Array Int
  , n :: Array Int
  , g :: Array Int
  , o :: Array Int
  }

type PersistedBoard =
  { id :: Int
  , name :: String
  , board :: Values
  }

toPersistedValue :: BoardValue -> Int
toPersistedValue (Number number) = number
toPersistedValue Empty = 0
toPersistedValue None = 999

fromPersistedValue :: Int -> BoardValue
fromPersistedValue 0 = Empty
fromPersistedValue 999 = None
fromPersistedValue n = Number n

-- | Convert a `Board` to a representation to be persisted in IndexedDB
toPersistentBoard :: Board -> PersistedBoard
toPersistentBoard { id, name, board } =
  { id: fromMaybe 0 id
  , name: name
  , board:
      { b: maybe [] (map $ toPersistedValue <<< snd) $ M.lookup B board
      , i: maybe [] (map $ toPersistedValue <<< snd) $ M.lookup I board
      , n: maybe [] (map $ toPersistedValue <<< snd) $ M.lookup N board
      , g: maybe [] (map $ toPersistedValue <<< snd) $ M.lookup G board
      , o: maybe [] (map $ toPersistedValue <<< snd) $ M.lookup O board
      }
  }

-- | Create a `Board` from it's IndexedDB-persisted representation
fromPersistentBoard :: PersistedBoard -> Board
fromPersistentBoard { id, name, board: values } =
  { id: Just id
  , name: name
  , board: M.fromFoldable
      [ Tuple B (map (Tuple false <<< fromPersistedValue) values.b)
      , Tuple I (map (Tuple false <<< fromPersistedValue) values.i)
      , Tuple N (zip [ false, false, true, false, false ] $ map fromPersistedValue values.n)
      , Tuple G (map (Tuple false <<< fromPersistedValue) values.g)
      , Tuple O (map (Tuple false <<< fromPersistedValue) values.o)
      ]
  }

-- | Prepare the IndexedDB database
-- |
-- | - Create the database
-- |
-- | - Create the `boards` table in the database
foreign import openDB :: Effect (Promise Unit)

-- | Save the board in the database
foreign import saveBoard :: EffectFn1 PersistedBoard (Promise Unit)

-- | Load all the boards from the database
foreign import loadBoards :: Effect (Promise (Array PersistedBoard))

-- | Delete a board from the database using its record id
foreign import deleteBoard :: EffectFn1 Int (Promise Unit)
