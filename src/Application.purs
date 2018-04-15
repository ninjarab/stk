module Application where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import Data.Either.Nested (Either5)
import Data.Functor.Coproduct.Nested (Coproduct5)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Helpers (class_)
import Network.HTTP.Affjax as AX

import Chart as Chart
import Navbar as Navigation
import Quote as Quote
import Typeahead.Container as Typeahead
import Summary as Summary

type State = { symbol :: String }

data Query a = HandleSelection Typeahead.Message a

type ChildQuery = Coproduct5 Navigation.Query Quote.Query Typeahead.Query Summary.Query Chart.Query

type ChildSlot = Either5 Unit Unit Unit Unit Unit

type Eff eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( Eff eff ) m => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { symbol: "" }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
    [ HH.slot' CP.cp1 unit Navigation.component unit absurd
    , HH.slot' CP.cp2 unit Quote.component state.symbol absurd
    , HH.slot' CP.cp3 unit Typeahead.component unit (HE.input HandleSelection)
    , HH.div
      [ class_ "section" ]
      [ HH.div
        [ class_ "container" ]
        [ HH.div
          [ class_ "columns is-desktop"]
          [ HH.div
            [ class_ "column is-one-third-desktop" ]
            [ HH.slot' CP.cp4 unit Summary.component state.symbol absurd ]
          , HH.div
            [ class_ "column is-two-third-desktop" ]
            [ HH.slot' CP.cp5 unit Chart.component state.symbol absurd ]
          ]
        ]
      ]
    , HH.footer
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
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval = case _ of
    HandleSelection (Typeahead.Selected item) next -> do
      H.liftAff $ log $ "Selected item from grand child " <> item
      H.modify (_ { symbol = item })
      pure next
