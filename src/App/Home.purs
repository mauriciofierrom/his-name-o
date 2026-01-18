-- | Home component
-- |
-- | The main container of the application
module App.Home (component) where

import App.Game (Direction(..), WinCondition(..), checkWinCondition, winConditions)
import Data.Tuple (Tuple(..))
import Prelude

import Web.UIEvent.KeyboardEvent (code)
import App.Board (Board, Id, parseNumber, updateMark)
import App.Components.Board as Board
import App.Persistence (openDB, loadBoards, fromPersistentBoard)
import Control.Promise (toAffE)
import Data.Array (filter, find, head, null, tail, sortWith, (:), (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type BoardState =
  { winner :: Boolean
  , board :: Board
  }

type State =
  { boards :: Array BoardState
  , showModal :: Boolean
  , currentBoard :: Maybe Board
  , number :: String
  , pastNumbers :: Array String
  , winCondition :: WinCondition
  }

data Action
  = Initialize
  | LoadBoards
  | OpenModal
  | CloseModal
  | UpdateNumber String
  | AddNumber
  | UndoAddNumber
  | ClearBoards
  | SetWinCondition WinCondition
  | HandleBoard Board.Output
  | None

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { boards: []
      , showModal: false
      , currentBoard: Nothing
      , number: ""
      , pastNumbers: []
      , winCondition: Line Horizontal
      }
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
  None -> pure unit
  Initialize -> do
    _ <- H.liftAff $ toAffE openDB
    handleAction LoadBoards
  LoadBoards -> do
    newBoards <- H.liftAff $ toAffE loadBoards
    H.modify_ \st -> st { boards = map (\board -> { winner: false, board: fromPersistentBoard board }) newBoards }
  OpenModal -> H.modify_ \st -> st { showModal = true }
  CloseModal -> do
    H.modify_ \st -> st { showModal = false, currentBoard = Nothing }
    handleAction LoadBoards
  UpdateNumber value -> do
    H.modify_ \st -> st { number = value }
  AddNumber -> do
    st <- H.get
    for_ (parseNumber st.number) $ \(Tuple letter num) -> do
      let markedBoards = map (\bs -> bs { board = updateMark st.number bs.board }) st.boards
      H.modify_
        ( _
            { boards = sortByBoardWinner $ checkedBoards letter num markedBoards st
            , pastNumbers = st.number : st.pastNumbers
            , number = ""
            }
        )
  UndoAddNumber -> do
    state <- H.get
    let mbLastNumber = head state.pastNumbers
    for_ mbLastNumber \lastNumber ->
      for_ (parseNumber lastNumber) \(Tuple letter num) -> do
        let markedBoards = map (\bs -> bs { board = updateMark lastNumber bs.board }) state.boards
        H.modify_
          ( _
              { boards = sortByBoardWinner $ checkedBoards letter num markedBoards state
              , pastNumbers = fromMaybe [] $ tail state.pastNumbers
              }
          )
  ClearBoards -> do
    H.modify_ \st -> st
      { number = ""
      , pastNumbers = []
      }
    handleAction LoadBoards
  SetWinCondition wc ->
    H.modify_ \st -> st { winCondition = wc }
  HandleBoard (Board.StartEdition id) ->
    H.modify_ \st -> st
      { currentBoard = _.board <$> find ((==) (Just id) <<< _.board.id) st.boards
      , showModal = true
      }
  HandleBoard Board.CancelEdition -> handleAction CloseModal
  HandleBoard (Board.Deleted id) ->
    H.modify_ \st -> st { boards = filter (\bs -> bs.board.id /= Just id) st.boards }
  where
  checkedBoards letter num markedBoards st =
    map
      ( \bs ->
          bs
            { winner = checkWinCondition letter num st.winCondition bs.board
            }
      )
      markedBoards

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (ClassName "container mx-auto p-4") ]
    [ HH.div
        [ HP.class_ (ClassName "flex justify-between items-center mb-6") ]
        [ HH.h1
            [ HP.class_ (ClassName "text-3xl font-bold") ]
            [ HH.text "His-name-O!" ]
        , if null state.boards then HH.text "" else inputNumber state
        , winConditionPicker
        , HH.button
            [ HP.class_ (ClassName "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded")
            , HE.onClick \_ -> OpenModal
            ]
            [ HH.text "Create New Board" ]
        ]
    , renderBoards state
    , if state.showModal then renderModal state else HH.text ""
    ]

inputNumber :: forall m. State -> MonadAff m => H.ComponentHTML Action Slots m
inputNumber state =
  HH.div
    [ HP.class_ (ClassName "flex items-center gap-3 my-4") ]
    [ HH.label
        [ HP.for "number"
        , HP.class_ (ClassName "font-medium text-gray-700")
        ]
        [ HH.text "Add number" ]
    , HH.input
        [ HP.type_ HP.InputText
        , HP.value state.number
        , HP.class_ (ClassName "px-3 py-3 w-40 border-2 border-gray-400 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500")
        , HE.onValueInput UpdateNumber
        , HE.onKeyDown \ke -> if code ke == "Enter" then AddNumber else None
        ]
    , HH.button
        [ HP.class_ (ClassName "px-4 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2")
        , HE.onClick \_ -> AddNumber
        ]
        [ HH.text "Add!" ]
    , if null state.pastNumbers then HH.text "" else undoButton
    , if null state.pastNumbers then HH.text "" else clearBoardsButton
    ]

undoButton :: forall m. MonadAff m => H.ComponentHTML Action Slots m
undoButton =
  HH.button
    [ HE.onClick \_ -> UndoAddNumber
    ]
    [ HH.i [ HP.class_ (ClassName "bi bi-arrow-counterclockwise") ] [] ]

clearBoardsButton :: forall m. MonadAff m => H.ComponentHTML Action Slots m
clearBoardsButton =
  HH.button
    [ HE.onClick \_ -> ClearBoards
    ]
    [ HH.i [ HP.class_ (ClassName "bi bi-radioactive") ] [] ]

winConditionPicker :: forall m. MonadAff m => H.ComponentHTML Action Slots m
winConditionPicker =
  HH.div
    [ HP.class_ (ClassName "flex gap-2") ]
    [ HH.label
        [ HP.class_ (ClassName "font-medium text-gray-700")
        ]
        [ HH.text "Win Condition" ]
    , HH.select
        [ HE.onSelectedIndexChange \i ->
            SetWinCondition $ fromMaybe (Line Horizontal) $ winConditions !! i
        ]
        (map winSelectionPickerOption winConditions)
    ]

winSelectionPickerOption :: forall m. MonadAff m => WinCondition -> H.ComponentHTML Action Slots m
winSelectionPickerOption wc =
  HH.option
    [ HP.value (show wc)
    ]
    [ HH.text (show wc) ]

renderBoards :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderBoards state =
  HH.div
    [ HP.class_ (ClassName "flex flex-row flex-wrap gap-6") ]
    (map renderBoard state.boards)

renderBoard :: forall m. MonadAff m => BoardState -> H.ComponentHTML Action Slots m
renderBoard { board, winner } =
  HH.slot _board (fromMaybe 0 board.id) Board.component (Board.View board winner) HandleBoard

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

sortByBoardWinner :: Array BoardState -> Array BoardState
sortByBoardWinner = sortWith (not <<< _.winner)
