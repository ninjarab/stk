module Quote where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)

import Data.Array (head)
import Data.Either (Either(..))
import Data.Fixed (Fixed, P10000, fromNumber, toNumber)
import Data.Foldable (traverse_)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Helpers (class_)
import Network.HTTP.Affjax as AX

newtype Quote = Quote
  { symbol :: String
  , companyName :: String
  , primaryExchange :: String
  , sector :: String
  , calculationPrice :: String
  , open :: Number
  , openTime :: Number
  , close :: Number
  , closeTime :: Number
  , high :: NullOrUndefined Number
  , low :: NullOrUndefined Number
  , latestPrice :: Number
  , latestSource :: String
  , latestTime :: String
  , latestUpdate :: Number
  , latestVolume :: Int
  , iexRealtimePrice :: NullOrUndefined Number
  , iexRealtimeSize :: NullOrUndefined Int
  , iexLastUpdated :: NullOrUndefined Number
  , delayedPrice :: Number
  , delayedPriceTime :: Number
  , previousClose :: Number
  , change :: Number
  , changePercent :: Number
  , iexMarketPercent :: NullOrUndefined Number
  , iexVolume :: NullOrUndefined Int
  , avgTotalVolume :: Int
  , iexBidPrice :: NullOrUndefined Number
  , iexBidSize :: NullOrUndefined Int
  , iexAskPrice :: NullOrUndefined Number
  , iexAskSize :: NullOrUndefined Int
  , marketCap :: Number
  , peRatio :: NullOrUndefined Number
  , week52High :: Number
  , week52Low :: Number
  , ytdChange :: Number
  }

derive instance repGenericQuote :: Generic Quote _
instance decodeQuote :: Decode Quote where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeQuote :: Encode Quote where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

type Input = String

type State =
  { loading :: Boolean
  , result :: Maybe Quote
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
      HH.div_
      [ HH.section
        [ class_ "custom-section" ]
        [ HH.div
          [ class_ "container" ]
          [ HH.div_
            [ HH.div
              [ class_ "columns is-mobile" ]
              [ HH.div
                [ class_ "column is-half is-offset-one-quarter" ]
                case state.result of
                  Nothing ->
                    [ HH.div
                      [ class_ "empty-box" ]
                      [ ]
                    ]
                  Just (Quote { companyName, symbol, latestPrice, change, changePercent, latestSource, latestTime }) ->
                    [ HH.div
                      [ class_ "content" ]
                      [ HH.p [ class_ "is-size-5 has-text-weight-bold" ] [ HH.text $ companyName <> " (" <> symbol <> ")" ]
                      , HH.p_
                        [ HH.span [ class_ "is-size-1 has-text-weight-bold"] [ HH.text $ show latestPrice ]
                        , HH.span [ class_ $ classAgainstPercent change] [ HH.text $ formatNumber change <> show change ]
                        , formatPercent changePercent
                        ]
                      , HH.p [ class_ "is-size-7 is-grey" ] [ HH.text $ latestSource <> " as of " <> latestTime ]
                      ]
                    ]
              ]
            ]
          ]
        ]
      ]
      where
        classAgainstPercent i =
          let color = case compare i 0.0 of
                        GT -> "is-green"
                        LT -> "is-red"
                        EQ -> "is-grey"
          in color <> " is-size-3"

        formatNumber n = if n >= 0.0 then "+" else ""

        formatPercent percent =
          let i = toNumber $ fromNumber percent * fromNumber 100.0 :: Fixed P10000
              c = classAgainstPercent i
          in HH.span [ class_ c ] [ HH.text $ "(" <> formatNumber i <> show i <> "%)" ]

    eval :: Query ~> DSL Query m
    eval = case _ of
      HandleSymbol s next -> do
        H.liftAff $ log $ "Received symbol for quote " <> s

        oldState <- H.get

        when (oldState.symbol /= s) do
          H.modify (_ { loading = true, symbol = s })
          case (head $ split (Pattern " - ") s) of
            Nothing -> pure unit
            Just symbol -> do
              response <- H.liftAff $ AX.get $ "https://api.iextrading.com/1.0/stock/" <> symbol <> "/quote"
              case runExcept $ decode =<< decodeJSON response.response of
                Left err -> do
                  H.liftAff $ traverse_ (log <<< renderForeignError) err
                  pure unit
                Right something ->
                  H.modify (_ { loading = false, result = Just something })

        pure next
