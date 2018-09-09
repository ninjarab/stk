module Models
  ( AllCharts(..)
  , Indice(..)
  , KeyStats(..)
  , OneDayChart(..)
  , Previous(..)
  , Quote(..)
  , Stats(..)
  , Symbol(..)
  , SystemEvent(..)
  , readKeyStatsJSON
  )
  where

import Prelude
import Data.Maybe (Maybe)
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign as Foreign
import Simple.JSON as JSON

type AllCharts =
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

type Indice =
  { label :: String
  , change :: Number
  }

type OneDayChart =
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
  , changeOverTime :: Maybe Number
  , marketChangeOverTime :: Maybe Number
  }

type Previous =
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

type Quote =
  { symbol :: String
  , companyName :: String
  , primaryExchange :: String
  , sector :: String
  , calculationPrice :: String
  , open :: Number
  , openTime :: Number
  , close :: Number
  , closeTime :: Number
  , high :: Maybe Number
  , low :: Maybe Number
  , latestPrice :: Number
  , latestSource :: String
  , latestTime :: String
  , latestUpdate :: Number
  , latestVolume :: Maybe Number
  , iexRealtimePrice :: Maybe Number
  , iexRealtimeSize :: Maybe Int
  , iexLastUpdated :: Maybe Number
  , delayedPrice :: Number
  , delayedPriceTime :: Number
  , previousClose :: Number
  , change :: Number
  , changePercent :: Number
  , iexMarketPercent :: Maybe Number
  , iexVolume :: Maybe Int
  , avgTotalVolume :: Number
  , iexBidPrice :: Maybe Number
  , iexBidSize :: Maybe Int
  , iexAskPrice :: Maybe Number
  , iexAskSize :: Maybe Int
  , marketCap :: Number
  , peRatio :: Maybe Number
  , week52High :: Number
  , week52Low :: Number
  , ytdChange :: Number
  }

type Stats =
  { companyName :: String
  , marketcap :: Number
  , beta :: Number
  , week52high :: Number
  , week52low :: Number
  , week52change :: Number
  , dividendRate :: Number
  , dividendYield :: Number
  , exDividendDate :: Either Int String
  , latestEPS :: Number
  , latestEPSDate :: String
  , symbol :: String
  }

readEitherImpl
  :: forall a b
   . JSON.ReadForeign a
  => JSON.ReadForeign b
  => Foreign
  -> Foreign.F (Either a b)
readEitherImpl f
    = Left <$> JSON.readImpl f
  <|> Right <$> JSON.readImpl f

type KeyStats = { stats :: Stats, quote :: Quote }

readKeyStatsJSON :: String -> Either Foreign.MultipleErrors KeyStats
readKeyStatsJSON ks = runExcept do
  keyStats <- JSON.readJSON' ks
  exDividendDate <- readEitherImpl keyStats.stats.exDividendDate
  let stats = keyStats.stats { exDividendDate = exDividendDate }
  pure $ keyStats { stats = stats }

type Symbol =
  { symbol :: String
  , name :: String
  , isEnabled :: Boolean
  }

type SystemEvent =
  { systemEvent :: Maybe String
  , timestamp :: Maybe Number
  }
