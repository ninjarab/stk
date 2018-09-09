module Navbar where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import Data.Array ((:))
import Data.Either (Either(..))
import Data.Fixed (Fixed, P10000, fromNumber, toNumber)
import Data.Foreign (ForeignError)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))

import Halogen (AttrName(..))
import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import Helpers (class_)
import Models (Indice, SystemEvent)
import Network.HTTP.Affjax as AX
import Simple.JSON as JSON

type Indices = Array Indice

type GlobalData = { indices :: Indices, systemEvent :: SystemEvent }

type State =
  { loading :: Boolean
  , result :: Maybe GlobalData
  }

data Query a
  = Initialize a
  | Finalize a

type DSL q m = H.ComponentDSL State q Void m
type Component m = H.Component HH.HTML Query Unit Void m
type Effects eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( Effects eff ) m => Component m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.nav
      [ class_ "navbar", ARIA.label "navigation", ARIA.label "main navigation" ]
      [ HH.div
        [ class_ "navbar-brand" ]
        [ HH.a
            [ class_ "navbar-item has-text-weight-bold is-size-3", HP.href "#market" ]
            [ HH.span_ [ HH.text "Stk" ]
            , HH.img [ HP.src "logo.svg" ]
            ]
        , HH.div
          [ class_ "navbar-burger", HP.attr (AttrName "data-target") "navbar-burger" ]
          [ HH.span_ [ ]
          , HH.span_ [ ]
          , HH.span_ [ ]
          ]
        ]
      , HH.div
        [ class_ "navbar-menu", HP.id_ "navbar-burger" ]
        [ HH.div
          [ class_ "navbar-start" ]
          [ HH.a
            [ class_ "navbar-item", HP.href "#market" ]
            [ HH.text "Market" ]
          , HH.a
            [ class_ "navbar-item", HP.href "#stock" ]
            [ HH.text "Stocks" ]
          , HH.a
            [ class_ "navbar-item coming-soon" ]
            [ HH.text "Foreign Exchange"
            , HH.sup_ [ HH.text "soon" ]
            ]
          , HH.a
            [ class_ "navbar-item coming-soon" ]
            [ HH.text "Crypto Currencies"
            , HH.sup_ [ HH.text "soon" ]
            ]
          ]
        , HH.div
          [ class_ "navbar-end" ]
          case st.result of
            Nothing -> []
            Just ({ indices, systemEvent } :: GlobalData) ->
              (renderSystemEvent systemEvent : renderIndices indices)
          ]
      ]
      where
        renderSystemEvent (systemEvent :: SystemEvent) = do
          case systemEvent.systemEvent of
            Nothing ->
              HH.div
              [ class_ "navbar-item" ]
              [ HH.span
                [ class_ "is-red warning-sign" ]
                [ HH.text "Market is closed" ]
              ]
            Just value  ->
              HH.div
              [ class_ "navbar-item" ]
                case value of
                  "S" ->
                    [ HH.span
                      [ class_ "has-text-link warning-sign" ]
                      [ HH.text "Pre-market" ]
                    ]
                  "M" ->
                    [ HH.span
                      [ class_ "has-text-warning warning-sign" ]
                      [ HH.text "Post-market" ]
                    ]
                  "R" ->
                    [ HH.span
                      [ class_ "is-green warning-sign" ]
                      [ HH.text "Market is open" ]
                    ]
                  _ ->
                    [ HH.span
                      [ class_ "is-red warning-sign" ]
                      [ HH.text "Market is closed" ]
                    ]

        renderIndices indices = renderIndice <$> indices

        renderIndice (indice :: Indice) =
          HH.div
          [ class_ "navbar-item" ]
          [ HH.text indice.label
          , formatIndice indice.change
          ]

        formatIndice indice =
          let i = toNumber $ fromNumber indice * fromNumber 100.0 :: Fixed P10000
              c = case compare i 0.0 of
                    GT -> "is-green-up"
                    LT -> "is-red-down"
                    EQ -> "is-grey-eq"
          in HH.span [ class_ c ] [ HH.text $ show i <> "%" ]

  eval :: Query ~> DSL Query m
  eval = case _ of
    Initialize next -> do
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/DIA/app-global-data"
      let parsedResponse = handleResponse $ JSON.readJSON response.response
      H.modify (_ { loading = false, result = parsedResponse })
      pure next
    Finalize next -> do
      pure next

handleResponse :: Either (NonEmptyList ForeignError) GlobalData -> Maybe GlobalData
handleResponse r = do
  case r of
    Left err -> Nothing
    Right something -> Just something
