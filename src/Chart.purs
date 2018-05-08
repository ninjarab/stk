module Chart where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable as F
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode)
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import ECharts.Commands as E
import ECharts.Monad (interpret, DSL')
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Color as C
import Helpers (class_)
import Models (OneDayChart(..), AllCharts(..))
import Network.HTTP.Affjax as AX

lineOptions ∷ String -> Array String -> Array Number -> DSL' ETP.OptionI
lineOptions symbol xAxis yAxis = do
  E.useUTC true
  E.tooltip do
    E.trigger ET.AxisTrigger
    E.animationEnabled false
    F.for_ (C.fromHexString "#11ffee00") E.backgroundColor
  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem xAxis

  E.yAxis do
    E.axisType ET.Value
    E.scale true
    E.splitLine $ E.lineStyle $ E.dashedLine

  E.series $ E.line do
    E.name symbol
    E.showSymbol false
    E.items $ map ET.numItem yAxis
    E.lineStyle do
      E.normalLineStyle $ E.color $ C.rgb 220 55 45
    E.itemStyle do
      E.normalItemStyle $ F.for_ (C.fromHexString "#FFFFFF") E.color
    E.symbol ET.Circle
  F.for_ (C.fromHexString "#0a0a0a") E.backgroundColor
  E.textStyle do
    F.for_ (C.fromHexString "#FFFFFF") E.color
  pure unit

type ChartData = Either (Array OneDayChart) (Array AllCharts)

type Input = String

type State =
  { index :: Int
  , symbol :: String
  , loading :: Boolean
  , result :: Maybe ChartData
  , unitOfTime :: String
  }

data UnitOfTime
  = Day
  | Month
  | Year
  | YearToDate

data Query a
  = HandleSymbol String a
  | HandleEChartsMessage Int EC.EChartsMessage a
  | Range Int UnitOfTime a

type AppEffects eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

type Component m = H.Component HH.HTML Query Input Void m
type HTML q m = H.ParentHTML Query q Int m
type DSL q m = H.ParentDSL State Query q Int Void m

component :: ∀ eff m. MonadAff ( AppEffects eff ) m => Component m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input HandleSymbol
    }
  where
    initialState ∷ Input -> State
    initialState i = { index: 1, symbol: i, loading: false, result: Nothing, unitOfTime: "1m" }

    render :: State -> HTML EC.EChartsQuery m
    render state =
      case state.result of
        Nothing ->
          HH.div_ []
        Just chartData ->
          HH.div_
          [ HH.h1
            [ class_ "is-size-3 has-text-centered" ]
            [ HH.text "Chart" ]
          , HH.slot state.index (EC.echarts Nothing) ({width: 880, height: 660} /\ unit)
              (Just <<< H.action <<< HandleEChartsMessage state.index)
          , HH.div
            [ class_ "columns" ]
            [ HH.div
              [ class_ "column is-half is-offset-one-quarter" ]
              [ HH.div
                [ class_ "buttons" ]
                [ HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 1 Day)), HP.disabled $ isDisabled "1d" state.unitOfTime ]
                  [ HH.text "1d"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 1 Month)), HP.disabled $ isDisabled "1m" state.unitOfTime ]
                  [ HH.text "1m"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 3 Month)), HP.disabled $ isDisabled "3m" state.unitOfTime ]
                  [ HH.text "3m"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 6 Month)), HP.disabled $ isDisabled "6m" state.unitOfTime ]
                  [ HH.text "6m"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 0 YearToDate)), HP.disabled $ isDisabled "ytd" state.unitOfTime ]
                  [ HH.text "ytd"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 1 Year)), HP.disabled $ isDisabled "1y" state.unitOfTime ]
                  [ HH.text "1y"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 2 Year)), HP.disabled $ isDisabled "2y" state.unitOfTime ]
                  [ HH.text "2y"]
                , HH.button
                  [ class_ "button", HE.onClick (HE.input_ (Range 5 Year)), HP.disabled $ isDisabled "5y" state.unitOfTime ]
                  [ HH.text "5y"]
                ]
              ]
            ]
          ]
          where
            isDisabled :: String -> String -> Boolean
            isDisabled unit unitOfTime = unit == unitOfTime

    eval :: Query ~> DSL EC.EChartsQuery m
    eval = case _ of
      HandleEChartsMessage ix EC.Initialized next -> do
        pure next
      HandleEChartsMessage ix (EC.EventRaised evt) next -> do
        pure next
      HandleSymbol s next -> do
        oldState <- H.get

        H.modify (_ { loading = true, symbol = s })

        response <- H.liftAff $ AX.get $ "https://api.iextrading.com/1.0/stock/" <> s <> "/chart/1m"

        case runExcept $ decode =<< decodeJSON response.response of
          Left err -> do
            H.liftAff $ F.traverse_ (log <<< renderForeignError) err
            pure unit
          Right something ->
            H.modify (_ { loading = false, result = Just (Right something), unitOfTime = "1m" })

        newState <- H.get

        case newState.result of
          Nothing -> pure unit
          Just chartData ->
            case chartData of
              Left oneDayData ->
                let labels = map (\(OneDayChart { label }) -> label) oneDayData
                    values = map (\(OneDayChart { average }) -> average) oneDayData
                in void $ H.query newState.index $ H.action $ EC.Set $ interpret $ lineOptions newState.symbol labels values
              Right allCharstData ->
                let labels = map (\(AllCharts { label }) -> label) allCharstData
                    values = map (\(AllCharts { close }) -> close) allCharstData
                in void $ H.query newState.index $ H.action $ EC.Set $ interpret $ lineOptions newState.symbol labels values

        pure next

      Range value unitOfTime next -> do
        oldState <- H.get
        u <- case unitOfTime of
              Day -> pure "d"
              Month -> pure "m"
              Year -> pure "y"
              YearToDate -> pure "ytd"
        v <- if value == 0 then pure "" else pure $ show value
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.get $ "https://api.iextrading.com/1.0/stock/" <> oldState.symbol <> "/chart/" <> v <> u

        case unitOfTime of
          Day ->
            case runExcept $ decode =<< decodeJSON response.response of
              Left err -> do
                H.liftAff $ F.traverse_ (log <<< renderForeignError) err
                pure unit
              Right something ->
                let filtered = filter (\(OneDayChart { average }) -> average > 0.0)  something
                in H.modify (_ { loading = false, result = Just (Left filtered), unitOfTime = "1d" })
          _ ->
            case runExcept $ decode =<< decodeJSON response.response of
              Left err -> do
                H.liftAff $ F.traverse_ (log <<< renderForeignError) err
                pure unit
              Right something ->
                H.modify (_ { loading = false, result = Just (Right something), unitOfTime = v <> u })

        newState <- H.get

        case newState.result of
          Nothing -> pure unit
          Just chartData ->
            case chartData of
              Left oneDayData ->
                let labels = map (\(OneDayChart { label }) -> label) oneDayData
                    values = map (\(OneDayChart { average }) -> average) oneDayData
                in void $ H.query newState.index $ H.action $ EC.Set $ interpret $ lineOptions newState.symbol labels values
              Right allCharstData ->
                let labels = map (\(AllCharts { label }) -> label) allCharstData
                    values = map (\(AllCharts { close }) -> close) allCharstData
                in void $ H.query newState.index $ H.action $ EC.Set $ interpret $ lineOptions newState.symbol labels values

        pure next
