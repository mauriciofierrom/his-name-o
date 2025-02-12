module App.Home where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))
import Effect.Aff.Class (class MonadAff)
import App.Board

type State =
  {
    boards :: Array Board
  }

data Action

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { boards: [] }
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.class_ (ClassName "container mx-auto p-4") ]
    [ HH.div
        [ HP.class_ (ClassName "flex justify-between items-center mb-6") ]
        [ HH.h1
            [ HP.class_ (ClassName "text-3xl font-bold") ]
            [ HH.text "Bingo Boards" ]
        , HH.button
            [ HP.class_ (ClassName "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded")]
            [ HH.text "Create New Board" ]
        ]
    ]
