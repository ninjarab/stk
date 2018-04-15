module Typeahead.Component where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER)

import Data.Array (elemIndex, mapWithIndex, filter, take, (:))
import Data.Foldable (length, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, toLower)

import Halogen as H
import Halogen.ECharts as EC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import Helpers (class_)
import Network.HTTP.Affjax as AX

import Select as Select
import Select.Utils.Setters as Setters

type TypeaheadItem = String

type Effects eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

data Query a
  = Log String a
  | HandleInputContainer (Select.Message Query TypeaheadItem) a

type State =
  { items    :: Array TypeaheadItem
  , selected :: TypeaheadItem
  , keepOpen :: Boolean }

type Input = { items :: Array String, keepOpen :: Boolean }
data Message = Selected String

type ChildSlot = Unit
type ChildQuery eff = Select.Query Query TypeaheadItem eff

component :: ∀ m e
  . MonadAff ( Effects e ) m
 => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i.items, selected: "", keepOpen: i.keepOpen }

    render
      :: State
      -> H.ParentHTML Query (ChildQuery (Effects e)) ChildSlot m
    render st =
      HH.div
        [ class_ "columns is-mobile" ]
        [
          HH.div
          [ class_ "column is-half is-offset-one-quarter" ]
          [ HH.slot unit Select.component input (HE.input HandleInputContainer) ]
        ]

      where
        input =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.TextInput
          , items: st.items
          , render: renderInputContainer
          }

    eval
      :: Query
      ~> H.ParentDSL State Query (ChildQuery (Effects e)) ChildSlot Message m
    eval = case _ of
      Log str a -> a <$ do
        H.liftAff $ log str

      HandleInputContainer m a -> a <$ case m of
        Select.Emit q -> eval q

        Select.Searched search -> do
          st <- H.get
          let newItems = filterItems search st.items
              index = elemIndex search st.items
          _ <- H.query unit $ H.action $ Select.ReplaceItems newItems
          traverse_ (H.query unit <<< H.action <<< Select.Highlight <<< Select.Index) index
          H.liftAff $ log $ "New search: " <> search

        Select.Selected item -> do
          st <- H.get

          _ <- if st.keepOpen
               then pure Nothing
               else H.query unit $ H.action $ Select.SetVisibility Select.Off

          if length (filter ((==) item) st.items) > 0
            then H.modify _ { selected = item }
            else H.modify _
                  { items = ( item : st.items )
                  , selected = item }

          H.raise $ Selected item
          H.liftAff $ log $ "New item selected: " <> item

        otherwise -> pure unit

filterItems :: TypeaheadItem -> Array TypeaheadItem -> Array TypeaheadItem
filterItems str = filter (\i -> contains (Pattern (toLower str)) $ toLower i)

renderInputContainer :: ∀ e
  . Select.State TypeaheadItem e
 -> Select.ComponentHTML Query TypeaheadItem e
renderInputContainer state = HH.div_ [ renderInput, renderContainer ]
  where
    renderInput =
      HH.div
        [ class_ "field" ]
        [ HH.div
          [ class_ "control"]
          [
            HH.input $ Setters.setInputProps
            [ class_ "input is-medium", HP.placeholder "Start typing a symbol or a company..." ]
          ]
        ]

    renderContainer =
      HH.div [ class_ "field" ]
      $ if state.visibility == Select.Off then []
        else [ renderItems $ renderItem `mapWithIndex` (take 10 state.items) ]
      where
        renderItems html =
          HH.div
          ( Setters.setContainerProps
            [ class_ "dropdown is-active" ]
          )
          [ HH.div
            [ class_ "dropdown-menu", HP.id_ "dropdown-menu", ARIA.role "menu"  ]
            [
              HH.div
              [ class_ "dropdown-content" ]
              html
            ]
          ]

        renderItem index item =
          HH.a ( Setters.setItemProps index [ class_ "dropdown-item" ] ) [ HH.text item ]
