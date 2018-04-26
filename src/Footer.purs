module Footer where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Helpers (class_)

type State = Unit

data Query a = Unit a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      HH.footer
        [ class_ "footer" ]
        [ HH.div
          [ class_ "container" ]
          [ HH.div
            [ class_ "content has-text-centered" ]
            [ HH.p_
              [ HH.strong_ [ HH.text "Stk"]
              , HH.span_ [ HH.text " by " ]
              , HH.a [ HP.href "https://mehdi-beddiaf.com" ] [ HH.text "Mehdi Beddiaf." ]
              , HH.span_ [ HH.text " Powered by " ]
              , HH.a [ HP.href "http://www.purescript.org/" ] [ HH.text "PureScript."]
              , HH.span_ [ HH.text " Data provided for free by " ]
              , HH.a [ HP.href "https://iextrading.com/developer" ] [ HH.text "iEx." ]
              ]
            ]
          ]
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Unit a) = pure a
