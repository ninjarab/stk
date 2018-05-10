module Market where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Fixed (Fixed, P10000, fromNumber, toNumber)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (decode)
import Data.Foreign.Generic (decodeJSON)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Helpers (class_)
import Models (Quote(..))
import Network.HTTP.Affjax as AX
import Routing.Hash (setHash)

type Quotes = Array Quote

type State =
  { gainersLoading :: Boolean
  , losersLoading :: Boolean
  , mostActiveLoading :: Boolean
  , gainers :: Maybe Quotes
  , losers :: Maybe Quotes
  , mostActive :: Maybe Quotes
  }

data Query a
  = Initialize a
  | Finalize a
  | Redirect String a

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
    initialState =
      { gainersLoading: false
      , losersLoading: false
      , mostActiveLoading: false
      , gainers: Nothing
      , losers: Nothing
      , mostActive: Nothing
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
      [ class_ "section" ]
      [ HH.div
        [ class_ "container" ]
        [ HH.div
          [ class_ "columns is-desktop" ]
          [ case state.mostActive of
              Nothing -> (renderNoDataAvailable "Most Active")
              Just mostActives -> ( renderTable "Most Active" mostActives )
          , case state.gainers of
              Nothing -> (renderNoDataAvailable "Gainers")
              Just gainers -> ( renderTable "Gainers" gainers )
          , case state.losers of
              Nothing -> (renderNoDataAvailable "Losers")
              Just losers -> ( renderTable "Losers" losers )
          ]
        ]
      ]
      where
        renderNoDataAvailable title =
          HH.div
          [ class_ "column" ]
          [ HH.div_
            [ HH.h1
              [ class_ "is-size-3 has-text-centered" ]
              [ HH.text title ]
            , HH.h2
              [ class_ "has-text-centered" ]
              [ HH.text "No data available" ]
            ]
          ]

        renderTable title quotes =
          HH.div
            [ class_ "column" ]
            [ HH.div_
              [ HH.h1
                [ class_ "is-size-3 has-text-centered" ]
                [ HH.text title ]
              , HH.table
                [ class_ "table is-fullwidth is-hoverable summary" ]
                [ HH.thead_
                  [ HH.tr_
                    [ HH.td [ class_ "has-text-left" ] [ HH.text "Symbol" ]
                    , HH.td [ class_ "has-text-centered" ] [ HH.text "Change" ]
                    , HH.td [ class_ "has-text-right" ] [ HH.text "Last Price" ]
                    ]
                  ]
                , HH.tbody_
                  case state.losers of
                    Nothing ->
                      [
                        HH.tr_ [ HH.text "No data available" ]
                      ]
                    Just losers -> ( renderBody quotes )
                ]
              ]
            ]

        renderBody quotes = renderRow <$> quotes

        renderRow (Quote quote) =
          HH.tr_
            [ HH.td
              [ class_ "has-text-left" ]
              [ HH.a [ HE.onClick (HE.input_ (Redirect quote.symbol)) ] [ HH.text quote.symbol ]
              , HH.p [ class_ "has-text-grey is-size-7" ] [ HH.text quote.companyName ]
              ]
            , HH.td [ class_ "has-text-centered" ]
              [ HH.span [ class_ $ classAgainstPercent quote.change] [ HH.text $ formatNumber quote.change <> show quote.change ]
              , formatPercent quote.changePercent
              ]
            , HH.td [ class_ "has-text-right" ] [ HH.text $ show quote.latestPrice ]
            ]

        classAgainstPercent i = do
          case compare i 0.0 of
            GT -> "is-green"
            LT -> "is-red"
            EQ -> "is-grey"

        formatNumber n = if n >= 0.0 then "+" else ""

        formatPercent percent =
          let i = toNumber $ fromNumber percent * fromNumber 100.0 :: Fixed P10000
              c = classAgainstPercent i
          in HH.span [ class_ c ] [ HH.text $ "(" <> formatNumber i <> show i <> "%)" ]

    eval :: Query ~> DSL Query m
    eval = case _ of
      Initialize next -> do
        H.modify (_ { mostActiveLoading = true })
        mostActiveResponse <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/market/list/mostactive"
        let parsedMostActiveResponse = handleResponse $ runExcept $ decode =<< decodeJSON mostActiveResponse.response
        H.modify (_ { mostActive = parsedMostActiveResponse, mostActiveLoading = false })

        H.modify (_ { gainersLoading = true })
        gainersResponse <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/market/list/gainers"
        let parsedGainersResponse = handleResponse $ runExcept $ decode =<< decodeJSON gainersResponse.response
        H.modify (_ { gainers = parsedGainersResponse, gainersLoading = false })

        H.modify (_ { losersLoading = true })
        losersResponse <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/market/list/losers"
        let parsedLosersResponse = handleResponse $ runExcept $ decode =<< decodeJSON losersResponse.response
        H.modify (_ { losers = parsedLosersResponse, losersLoading = false })

        pure next

      Finalize next -> do
        pure next

      Redirect symbol next -> do
        H.liftEff $ setHash $ "stock/" <> symbol
        pure next

handleResponse :: Either (NonEmptyList ForeignError) Quotes -> Maybe Quotes
handleResponse r = do
  case r of
    Left err -> Nothing
    Right something -> Just something
