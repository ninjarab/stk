module Navbar where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Fixed (Fixed, P10000, fromNumber, toNumber)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))

import Halogen (AttrName(..))
import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import Helpers (class_)
import Network.HTTP.Affjax as AX

newtype Indice = Indice
  { label :: String
  , change :: Number
  }

derive instance repGenericIndice :: Generic Indice _
instance decodeIndice :: Decode Indice where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeIndice :: Encode Indice where
encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

type Indices = Array Indice

newtype GlobalData = GlobalData { indices :: Indices}

derive instance repGenericGlobalData :: Generic GlobalData _
instance decodeGlobalData :: Decode GlobalData where
decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

type State =
  { loading :: Boolean
  , result :: Maybe GlobalData
  }

data Query a
  = Initialize a
  | Finalize a
  | GetState (Boolean -> a)

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
        [ HH.h1
          [ class_ "title" ]
          [ HH.a
            [ class_ "navbar-item" ]
            [ HH.text "Stk"]
          ],
          HH.div
          [ class_ "navbar-burger", HP.attr (AttrName "data-target") "navBarBurger" ]
          [ HH.span_ [ ]
          , HH.span_ [ ]
          , HH.span_ [ ]
          ]
        ],
        HH.div
        [ HP.id_ "navBarBurger", class_ "navbar-menu" ]
        [ HH.div
          [ class_ "navbar-end" ]
          case st.result of
            Nothing -> []
            Just (GlobalData { indices }) -> ( renderIndices indices )
        ]
      ]
      where
        renderIndices indices = renderIndice <$> indices

        renderIndice (Indice indice) =
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
      H.liftAff $ log "Fetching market data on mount"
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/stock/DIA/app-global-data"
      let parsedResponse = handleResponse $ runExcept $ decode =<< decodeJSON response.response
      H.modify (_ { loading = false, result = parsedResponse })
      pure next
    Finalize next -> do
      pure next
    GetState reply -> do
      state <- H.get
      pure (reply state.loading)

handleResponse :: Either (NonEmptyList ForeignError) GlobalData -> Maybe GlobalData
handleResponse r = do
  case r of
    Left err -> Nothing
    Right something -> Just something
