module Crypto where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Data.Array (mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (renderForeignError)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Models (Quote)
import Network.HTTP.Affjax as AX
import Quote as Quote
import Simple.JSON as JSON
import Helpers (class_)

type Quotes = Array Quote

type State =
  { loading :: Boolean
  , quotes :: Maybe Quotes
  }

data Query a
  = Initialize a
  | Finalize a

type Input = Unit

type Output = Void

newtype Slot = Slot Int
derive newtype instance eqSlot :: Eq Slot
derive newtype instance ordSlot :: Ord Slot

type Component m = H.Component HH.HTML Query Input Output m

type CustomEff eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( CustomEff eff ) m => Component m
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { loading: false, quotes: Nothing }

    render :: State -> H.ParentHTML Query Quote.Query Slot m
    render state =
      case state.quotes of
        Nothing ->
          HH.div_ []
        Just (quotes :: Quotes) ->
          HH.div [ class_ "columns is-multiline is-desktop has-text-centered" ] $ mapWithIndex (\(i :: Int) ({ symbol } :: Quote ) -> renderQuote i symbol)  quotes

      where
        renderQuote i s =
          let c = case i `mod` 2 of
                    0 -> "column is-4 is-offset-2"
                    _ -> "column is-4"
          in
            HH.div
              [ class_ c ]
              [ HH.slot (Slot i) Quote.component (Just s) absurd ]

    eval :: Query ~> H.ParentDSL State Query Quote.Query Slot Output m
    eval = case _ of
      Initialize next -> do
        H.modify (_ { loading = true })
        cryptoResponse <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/market/crypto"

        case JSON.readJSON cryptoResponse.response of
          Left err -> do
            H.liftAff $ traverse_ (log <<< renderForeignError) err
            pure unit
          Right crypto ->
            H.modify (_ { quotes = (Just crypto) })

        H.modify (_ { loading = false })

        pure next
      Finalize next -> do
        pure next
