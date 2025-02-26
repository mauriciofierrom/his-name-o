-- | The Board component
-- |
-- | It supports display and edit modes and can be hosted in parent components.
module App.Components.Board
  ( component
  , Input(..)
  , Output(..)
  ) where

import Prelude

import Control.Promise (toAffE)
import App.Board (Board, BoardValue(..), Id, Index, Letter, emptyBoard, toString, updateValue)
import Data.Map (Map)
import Data.Map as M
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Array (concat, mapWithIndex)
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (runEffectFn1)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..))
import Halogen.HTML.Properties as HP
import App.Persistence (deleteBoard, saveBoard, toPersistentBoard)

-- | The component's inputs
-- |
-- | - `Create` - Load the component in edition mode for the creation of a new board
-- |
-- | - `Edit` - Load the component in edition mode for an existing/persisted board
-- |
-- | - `View` - Load the component in view mode
data Input
  = Create
  | Edit Board
  | View Board

-- | The component's outputs
-- |
-- | - `CancelEdition` - Signals that we're canceling edition mode
-- |
-- | - `StartEdition` - Signals that we've enable edition mode for the provided board
-- |
-- | - `Deleted` - Signals that a board has been deleted
data Output
  = CancelEdition
  | StartEdition Id
  | Deleted Id

type State =
  { board :: Board
  , mode :: Mode
  }

data Action
  = Cancel
  | Save
  | UpdateBoardName String
  | UpdateCell Letter Index String
  | TriggerEdition Index
  | Receive Input
  | Delete Id

data Mode
  = Display
  | Edition

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState Create = { board: emptyBoard, mode: Edition }
  initialState (View board) = { board, mode: Display }
  initialState (Edit board) = { board, mode: Edition }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Cancel -> do
    H.raise CancelEdition
  UpdateBoardName name ->
    H.modify_ \st -> st { board = st.board { name = name } }
  UpdateCell letter idx value ->
    H.modify_ \st -> st { board = updateValue letter idx value st.board }
  TriggerEdition id -> H.raise (StartEdition id)
  Save -> do
    state <- H.get
    _ <- H.liftAff $ toAffE $ runEffectFn1 saveBoard (toPersistentBoard state.board)
    H.raise CancelEdition
  Delete id -> do
    _ <- H.liftAff $ toAffE $ runEffectFn1 deleteBoard id
    H.raise $ Deleted id
  Receive Create -> pure unit
  Receive (Edit _board) -> pure unit
  Receive (View board) -> H.modify_ \st -> st { board = board }

render :: forall m. State -> H.ComponentHTML Action () m
render { board, mode } =
  HH.div
    [ HP.class_ (ClassName "bg-white rounded-lg shadow-md") ]
    [ HH.div
        [ HP.class_ (ClassName "px-4 py-3 bg-gray-50 border-b flex justify-between items-center") ]
        [ HH.h3
            [ HP.class_ (ClassName "text-lg font-semibold") ]
            [ case mode of
                Edition -> HH.input
                  [ HP.type_ HP.InputText
                  , HP.value board.name
                  , HP.class_ (ClassName "w-full px-3 py-2 border rounded-md")
                  , HE.onValueInput UpdateBoardName
                  ]
                Display -> HH.text board.name
            ]
        , HH.div
            [ HP.class_ (ClassName "flex gap-2") ]
            (displayActions mode board)
        ]
    , HH.div
        [ HP.class_ (ClassName "p-4") ]
        [ HH.div
            [ HP.class_ (ClassName "grid grid-flow-col grid-rows-5 gap-1") ]
            (renderCells mode board.board)
        ]
    , HH.div
        [ HP.class_ (ClassName "flex justify-end gap-2") ]
        (editActions mode)
    ]

displayActions :: forall m. Mode -> Board -> Array (H.ComponentHTML Action () m)
displayActions Edition _board = [ HH.text "" ]
displayActions Display board =
  [ HH.button
      [ HP.class_ (ClassName "hover:bg-blue-200 px-3 py-1 rounded text-blue-700")
      , HE.onClick \_ -> TriggerEdition (fromMaybe 0 board.id)
      ]
      [ HH.i [ HP.class_ (ClassName "bi bi-pencil-square") ] [] ]
  , HH.button
      [ HP.class_ (ClassName "hover:bg-red-200 px-3 py-1 rounded text-red-700")
      , HE.onClick \_ -> Delete (fromMaybe 0 board.id)
      ]
      [ HH.i [ HP.class_ (ClassName "bi bi-trash") ] [] ]
  ]

editActions :: forall m. Mode -> Array (H.ComponentHTML Action () m)
editActions Display = [ HH.text "" ]
editActions Edition =
  [ HH.button
      [ HP.class_ (ClassName "bg-gray-300 hover:bg-gray-400 px-4 py-2 rounded")
      , HE.onClick \_ -> Cancel
      ]
      [ HH.text "Cancel" ]
  , HH.button
      [ HP.class_ (ClassName "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded")
      , HE.onClick \_ -> Save
      ]
      [ HH.text "Save" ]
  ]

cellWrapper :: forall m. H.ComponentHTML Action () m -> H.ComponentHTML Action () m
cellWrapper inner =
  HH.div
    [ HP.class_ (ClassName "h-full w-full aspect-square border-none rounded flex items-center justify-center p-2 text-lg bg-gray-50") ]
    [ inner ]

blockedCell :: forall m. H.ComponentHTML Action () m
blockedCell = HH.i [ HP.class_ (ClassName "bi bi-x-lg") ] []

renderCellInput :: forall m. Letter -> Int -> BoardValue -> H.ComponentHTML Action () m
renderCellInput letter idx value =
  cellWrapper $
    case value of
      None -> blockedCell
      _ ->
        HH.input
          [ HP.type_ HP.InputText
          , HP.value (toString value)
          , HP.class_ (ClassName "w-full h-full text-center border-none rounded focus:border-transparent focus:ring-0 focus:outline-none focus:ring-2 focus:ring-blue-500")
          , HE.onValueInput (UpdateCell letter idx)
          ]

renderCell :: forall m. BoardValue -> H.ComponentHTML Action () m
renderCell value =
  cellWrapper $
    case value of
      Number number -> HH.text (show number)
      Empty -> HH.text ""
      None -> HH.i [ HP.class_ (ClassName "bi bi-x-lg") ] []

renderCells :: forall m. Mode -> Map Letter (Array BoardValue) -> Array (H.ComponentHTML Action () m)
renderCells Display board = map renderCell (concat $ L.toUnfoldable $ M.values board)
renderCells Edition board =
  foldrWithIndex (\letter values acc -> mapWithIndex (renderCellInput letter) values <> acc) [] board
