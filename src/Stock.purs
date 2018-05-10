module Stock where

import Prelude

import Chart as Chart
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Helpers (class_)
import Network.HTTP.Affjax as AX
import Quote as Quote
import Routing.Hash (getHash)
import Summary as Summary
import Typeahead.Container as Typeahead

type State = { symbol :: Maybe String }

data Query a
  = HandleSelection Typeahead.Message a
  | Receive (Maybe String) a
  | Initialize a
  | Finalize a

type Input = Maybe String

type Output = Void

type Component m = H.Component HH.HTML Query Input Output m

type ChildQuery = Coproduct4 Quote.Query Typeahead.Query Summary.Query Chart.Query

type ChildSlot = Either4 Unit Unit Unit Unit

type CustomEff eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( CustomEff eff ) m => Component m
component =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: HE.input Receive
    }
  where
    initialState :: Input -> State
    initialState i = { symbol: i }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state =
      HH.div_
      [ HH.slot' CP.cp1 unit Quote.component state.symbol absurd
      , HH.slot' CP.cp2 unit Typeahead.component unit (HE.input HandleSelection)
      , HH.div
        [ class_ "section" ]
        [ HH.div
          [ class_ "container" ]
          [ HH.div
            [ class_ "columns is-desktop"]
            [ HH.div
              [ class_ "column is-one-third-desktop" ]
              [ HH.slot' CP.cp3 unit Summary.component state.symbol absurd ]
            , HH.div
              [ class_ "column is-two-third-desktop" ]
              [ HH.slot' CP.cp4 unit Chart.component state.symbol absurd ]
            ]
          ]
        ]
      ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output m
    eval = case _ of
      Initialize next -> do
        h <- H.liftEff getHash
        case stripPrefix (Pattern "stock/") h of
          Nothing ->
            pure unit
          Just symbol -> do
            H.modify (_ { symbol = Just symbol })
        pure next
      Finalize next -> do
        pure next
      HandleSelection (Typeahead.Selected item) next -> do
        H.modify (_ { symbol = Just item })
        pure next
      Receive s next -> do
        H.modify (_ { symbol = s })
        pure next
