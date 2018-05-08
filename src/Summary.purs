module Summary where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Fixed (Fixed, P10000, fromNumber, toNumber)
import Data.Foldable (traverse_)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Formatter.Number (Formatter(..), format)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Helpers (class_)
import Models (Stats(..), Quote(..))
import Network.HTTP.Affjax as AX

fmt ∷ Formatter
fmt = Formatter
  { comma: true
  , before: 0
  , after: 0
  , abbreviations: false
  , sign: false
  }

newtype KeyStats = KeyStats
  { stats :: Stats
  , quote :: Quote
  }

derive instance repGenericKeyStats :: Generic KeyStats _
instance decodeKeyStats :: Decode KeyStats where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeKeyStats :: Encode KeyStats where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

type Input = String

type State =
  { loading :: Boolean
  , result :: Maybe KeyStats
  , symbol :: String
  }

data Query a = HandleSymbol String a

type DSL q m = H.ComponentDSL State q Void m
type Component m = H.Component HH.HTML Query Input Void m
type Effects eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component ::  ∀ eff m. MonadAff ( Effects eff ) m => Component m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleSymbol
    }
  where
    initialState :: Input -> State
    initialState i = { loading: false, result: Nothing, symbol: i }

    render :: State -> H.ComponentHTML Query
    render state =
      case state.result of
          Nothing ->
            HH.div_ []
          Just (KeyStats {stats : Stats s, quote : Quote q}) ->
            HH.div_
            [ HH.h1
              [ class_ "is-size-3 has-text-centered" ]
              [ HH.text "Summary" ]
            , HH.table
              [ class_ "table is-hoverable summary" ]
              [ HH.tbody_
                [ HH.tr_
                  [ HH.td_ [ HH.text "Volume" ]
                  , case unNullOrUndefined q.latestVolume of
                      Nothing ->
                        HH.td [ class_ "has-text-right" ] [ HH.text "0" ]
                      Just value ->
                        HH.td [ class_ "has-text-right" ] [ HH.text $ format fmt value ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Avg daily volume" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ format fmt q.avgTotalVolume ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Previous close" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ show q.previousClose ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "52 week range" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ show s.week52low <> " - " <> show s.week52high ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Market cap" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ formatMarketCap s.marketcap ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Beta" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ show $ formatNumber s.beta ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Latest EPS" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ show s.latestEPS ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Latest EPS date" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text s.latestEPSDate ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Dividend & yield" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text $ show s.dividendRate <> formatPercent s.dividendYield  ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "Ex-dividend date" ]
                  , HH.td [ class_ "has-text-right" ] [ HH.text s.exDividendDate ]
                  ]
                , HH.tr_
                  [ HH.td_ [ HH.text "P/E ratio" ]
                  , case unNullOrUndefined q.peRatio of
                      Nothing ->
                        HH.td [ class_ "has-text-right" ] [ HH.text "0" ]
                      Just value ->
                        HH.td [ class_ "has-text-right" ] [ HH.text $ show value ]
                  ]
                ]
              ]
            ]
      where
        formatNumber n = toNumber $ fromNumber n :: Fixed P10000

        formatPercent percent =
          let i = toNumber $ fromNumber percent * fromNumber 100.0 :: Fixed P10000
          in " (" <> show i <> "%)"

        formatMarketCap m =
          let n = m / 1000000000.0
          in if n < 1.0
          then (show $ formatNumber $ m / 1000000.0) <> "M"
          else (show $ formatNumber n) <> "B"

    eval :: Query ~> DSL Query m
    eval = case _ of
      HandleSymbol s next -> do
        oldState <- H.get

        H.modify (_ { loading = true, symbol = s })

        response <- H.liftAff $ AX.get $ "https://api.iextrading.com/1.0/stock/" <> s <> "/batch?types=stats,quote"

        case runExcept $ decode =<< decodeJSON response.response of
          Left err -> do
            H.liftAff $ traverse_ (log <<< renderForeignError) err
            pure unit
          Right something ->
            H.modify (_ { loading = false, result = Just something })

        pure next
