module Models
  ( AllCharts(..)
  , Indice(..)
  , OneDayChart(..)
  , Previous(..)
  , Quote(..)
  , Stats(..)
  , Symbol(..)
  , SystemEvent(..))
  where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)

newtype AllCharts = AllCharts
  { date :: String
  , open :: Number
  , high :: Number
  , low :: Number
  , close :: Number
  , volume :: Number
  , unadjustedVolume :: Number
  , change :: Number
  , changePercent :: Number
  , vwap :: Number
  , label :: String
  , changeOverTime :: Number
  }

derive instance repGenericAllCharts :: Generic AllCharts _
instance decodeAllCharts :: Decode AllCharts where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype Indice = Indice
  { label :: String
  , change :: Number
  }

derive instance repGenericIndice :: Generic Indice _
instance decodeIndice :: Decode Indice where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeIndice :: Encode Indice where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype OneDayChart = OneDayChart
  { date :: String
  , minute :: String
  , label :: String
  , high :: Number
  , low :: Number
  , average :: Number
  , volume :: Number
  , notional :: Number
  , numberOfTrades :: Number
  , marketHigh :: Number
  , marketLow :: Number
  , marketAverage :: Number
  , marketVolume :: Number
  , marketNotional :: Number
  , marketNumberOfTrades :: Number
  , changeOverTime :: NullOrUndefined Number
  , marketChangeOverTime :: NullOrUndefined Number
  }

derive instance repGenericOneDayChart :: Generic OneDayChart _
instance decodeOneDayChart :: Decode OneDayChart where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype Previous = Previous
  { symbol :: String
  , date :: String
  , open :: Number
  , high ::  Number
  , low :: Number
  , close :: Number
  , volume :: Number
  , unadjustedVolume :: Number
  , change :: Number
  , changePercent :: Number
  , vwap :: Number
  }

derive instance repGenericPrevious :: Generic Previous _
instance decodePrevious :: Decode Previous where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

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
  , latestVolume :: NullOrUndefined Number
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
  , avgTotalVolume :: Number
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

newtype Stats = Stats
  { companyName :: String
  , marketcap :: Number
  , beta :: Number
  , week52high :: Number
  , week52low :: Number
  , week52change :: Number
  , dividendRate :: Number
  , dividendYield :: Number
  , exDividendDate :: String
  , latestEPS :: Number
  , latestEPSDate :: String
  , symbol :: String
  }

-- newtype Stats = Stats
--   { companyName :: String
--   , marketcap :: Number
--   , beta :: Number
--   , week52high :: Number
--   , week52low :: Number
--   , week52change :: Number
--   , shortInterest :: Number
--   , shortDate :: String
--   , dividendRate :: Number
--   , dividendYield :: Number
--   , exDividendDate :: String
--   , latestEPS :: Number
--   , latestEPSDate :: String
--   , sharesOutstanding :: Number
--   , float :: Number
--   , returnOnEquity :: Number
--   , consensusEPS :: Number
--   , numberOfEstimates :: Number
--   , symbol :: String
--   , "EBITDA" :: Number
--   , revenue :: Number
--   , grossProfit :: Number
--   , cash :: Number
--   , debt :: Number
--   , ttmEPS :: Number
--   , revenuePerShare :: Number
--   , revenuePerEmployee :: Number
--   , peRatioHigh :: Number
--   , peRatioLow :: Number
--   , "EPSSurpriseDollar" :: NullOrUndefined Number
--   , "EPSSurprisePercent" :: Number
--   , returnOnAssets :: Number
--   , returnOnCapital :: NullOrUndefined Number
--   , profitMargin :: Number
--   , priceToSales :: Number
--   , priceToBook :: Number
--   , day200MovingAvg :: Number
--   , day50MovingAvg :: Number
--   , institutionPercent :: Number
--   , insiderPercent :: NullOrUndefined Number
--   , shortRatio :: Number
--   , year5ChangePercent :: Number
--   , year2ChangePercent :: Number
--   , year1ChangePercent :: Number
--   , ytdChangePercent :: Number
--   , month6ChangePercent :: Number
--   , month3ChangePercent :: Number
--   , month1ChangePercent :: Number
--   , day5ChangePercent :: Number
--   }

derive instance repGenericStats :: Generic Stats _
instance decodeStats :: Decode Stats where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeStats :: Encode Stats where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype Symbol = Symbol
  { symbol :: String
  , name :: String
  , date :: String
  , isEnabled :: Boolean
  , type :: String
  , iexId :: String
  }

derive instance repGenericSymbol :: Generic Symbol _
instance decodeSymbol :: Decode Symbol where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSymbol :: Encode Symbol where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

newtype SystemEvent = SystemEvent
  { systemEvent :: NullOrUndefined String
  , timestamp :: NullOrUndefined Number
  }

derive instance repGenericSystemEvent :: Generic SystemEvent _
instance decodeSystemEvent :: Decode SystemEvent where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSystemEvent :: Encode SystemEvent where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
