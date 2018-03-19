module Typeahead.Container where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foreign (ForeignError, readArray)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Network.HTTP.Affjax as AX
import Type.Data.Boolean (kind Boolean)
import Typeahead.Component as Typeahead

newtype Symbol = Symbol
  {
    symbol :: String
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

type Symbols = Array Symbol

type State =
  { loading :: Boolean
  , result :: Maybe Symbols
  }

type Input = Unit
type Message = Void

data Query a
  = Initialize a
  | Finalize a
  | HandleSelection Typeahead.Message a

type Component m = H.Component HH.HTML Query Unit Void m
type DSL q m = H.ParentDSL State Query q Unit Void m
type HTML q m = H.ParentHTML Query q Unit m

type Effects eff = ( ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM, now :: NOW, avar :: AVAR, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( Effects eff ) m => Component m
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
    initialState = { loading: false, result: Nothing }

    eval :: Query ~> DSL Typeahead.Query m
    eval = case _ of
      Initialize next -> do
        H.liftAff $ log "Fetching MostActive data on mount"
        H.modify (_ { loading = true })
        response <- H.liftAff $ AX.get "https://api.iextrading.com/1.0/ref-data/symbols"
        let parsedResponse = handleResponse $ runExcept $ traverse decode =<< readArray =<< decodeJSON response.response
        H.modify (_ { loading = false, result = parsedResponse })
        pure next
      Finalize next -> do
        pure next
      HandleSelection (Typeahead.Selected item) next -> do
        H.liftAff $ log $ "Selected item from child " <> item
        pure next

    render :: State -> HTML Typeahead.Query m
    render st =
      HH.div_
      [
        HH.nav
        [ class_ "navbar", ARIA.label "navigation", ARIA.label "main navigation" ]
        [
          HH.div
          [ class_ "navbar-brand" ]
          [
            HH.h1
            [ class_ "title" ]
            [
              HH.a
              [ class_ "navbar-item" ]
              [ HH.text "Stk"]
            ]
          ]
        ],
        HH.section
        [ class_ "section" ]
        [
          HH.div
          [ class_ "hero-body" ]
          [
            HH.div
              [ class_ "container" ]
              [
                HH.form_ $
                [ HH.p_
                  [ HH.text (if st.loading then "Fetching symbols..." else "") ]
                , HH.div_
                    case st.result of
                      Nothing ->
                        []
                      Just symbols ->
                        let config = { items: (map (\(Symbol { symbol, name }) -> symbol <> " - " <> name) symbols)
                                     , keepOpen: false
                                     }
                        in [HH.slot unit Typeahead.component config (HE.input HandleSelection)]
                ]
              ]
          ]
        ]
      ]

handleResponse :: Either (NonEmptyList ForeignError) Symbols -> Maybe Symbols
handleResponse r = do
  case r of
    Left err -> Nothing
    Right something -> Just something

class_ :: ∀ p i. String -> H.IProp ( "class" :: String | i ) p
class_ = HP.class_ <<< HH.ClassName
