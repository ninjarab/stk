module Router where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Routes
  = Crypto
  | Forex
  | Market
  | Stock

instance showRoutes :: Show Routes where
  show Crypto = "crypto"
  show Forex = "forex"
  show Market = "market"
  show Stock = "stock"

routing :: Match Routes
routing = crypto <|> forex <|> market <|> stock
  where
    crypto = Crypto <$ lit "crypto"
    forex = Forex <$ lit "forex"
    market = Market <$ lit "market"
    stock = Stock <$ lit "stock"
