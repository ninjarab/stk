module Router where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

data Routes
  = Crypto
  | Forex
  | Market
  | Stock
  | StockShow String

instance showRoutes :: Show Routes where
  show Crypto = "crypto"
  show Forex = "forex"
  show Market = "market"
  show Stock = "stock"
  show (StockShow s) = "stock " <> s

routing :: Match Routes
routing = crypto <|> forex <|> market <|> stockShow <|> stock
  where
    crypto = Crypto <$ lit "crypto"
    forex = Forex <$ lit "forex"
    market = Market <$ lit "market"
    stockShow = StockShow <$ lit "stock" <*> str
    stock = Stock <$ lit "stock"
