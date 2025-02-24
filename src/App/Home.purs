-- | Home component
-- |
-- | The main container of the application
module App.Home (component) where

import Prelude

import App.Board (Board, Id)
import App.Components.Board as Board
import App.Persistence (openDB, loadBoards, fromPersistentBoard)

import Control.Promise (toAffE)
import Data.Array (find, filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type State =
  { boards :: Array Board
  , showModal :: Boolean
  , currentBoard :: Maybe Board
  }

data Action
  = Initialize
  | LoadBoards
  | OpenModal
  | CloseModal
  | HandleBoard Board.Output

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { boards: [], showModal: false, currentBoard: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

type Slots = (board :: forall q. H.Slot q Board.Output Id)

_board = Proxy :: Proxy "board"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.liftAff $ toAffE openDB
    handleAction LoadBoards
  LoadBoards -> do
    newBoards <- H.liftAff $ toAffE loadBoards
    H.modify_ \st -> st { boards = map fromPersistentBoard newBoards }
  OpenModal -> H.modify_ \st -> st { showModal = true }
  CloseModal -> do
    H.modify_ \st -> st { showModal = false, currentBoard = Nothing }
    handleAction LoadBoards
  HandleBoard (Board.StartEdition id) ->
    H.modify_ \st -> st { currentBoard = find (\b -> b.id == Just id) st.boards, showModal = true }
  HandleBoard Board.CancelEdition -> handleAction CloseModal
  HandleBoard (Board.Deleted id) ->
    H.modify_ \st -> st { boards = filter (\b -> b.id /= Just id) st.boards }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (ClassName "container mx-auto p-4") ]
    [ HH.div
        [ HP.class_ (ClassName "flex justify-between items-center mb-6") ]
        [ HH.h1
            [ HP.class_ (ClassName "text-3xl font-bold") ]
            [ HH.text "Bingo Boards" ]
        , HH.button
            [ HP.class_ (ClassName "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded")
            , HE.onClick \_ -> OpenModal
            ]
            [ HH.text "Create New Board" ]
        ]
    , renderBoards state
    , if state.showModal then renderModal state else HH.text ""
    ]

renderBoards :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderBoards state =
  HH.div
    [ HP.class_ (ClassName "flex flex-row flex-wrap gap-6") ]
    (map renderBoard state.boards)

renderBoard :: forall m. MonadAff m => Board -> H.ComponentHTML Action Slots m
renderBoard board =
  HH.slot _board (fromMaybe 0 board.id) Board.component (Board.View board) HandleBoard

renderModal :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderModal state =
  HH.div
    [ HP.class_ (ClassName "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50") ]
    [ HH.div
        [ HP.class_ (ClassName "bg-white rounded-lg p-6 max-w-2xl w-full m-4") ]
        [ HH.div
            [ HP.class_ (ClassName "flex justify-between items-center mb-4") ]
            [ HH.h2
                [ HP.class_ (ClassName "text-2xl font-bold") ]
                [ HH.text case state.currentBoard of
                    Just _ -> "Edit Board"
                    _ -> "Create New Board"
                ]
            , HH.button
                [ HP.class_ (ClassName "text-gray-500 hover:text-gray-700 text-2xl")
                , HE.onClick \_ -> CloseModal
                ]
                [ HH.text "×" ]
            ]
        , case state.currentBoard of
            Just currentBoard -> HH.slot _board 999 Board.component (Board.Edit currentBoard) HandleBoard
            Nothing -> HH.slot _board 999 Board.component Board.Create HandleBoard
        ]
    ]
