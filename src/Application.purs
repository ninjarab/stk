module Application (component, matchRoutes, Query) where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.ECharts as EC
import Halogen.HTML as HH

import Router as RT
import Routing.Hash (matches)
import Network.HTTP.Affjax as AX

import Footer as Footer
import Market as Market
import Navbar as Navigation
import Stock as Stock

type State = RT.Routes

data Query a = GOTO RT.Routes a

type Input = Unit

type Output = Void

type Component m = H.Component HH.HTML Query Input Output m

type ChildQuery = Coproduct4 Navigation.Query Market.Query Stock.Query Footer.Query

type ChildSlot = Either4 Unit Unit Unit Unit

type CustomEff eff = EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER | eff )

component :: ∀ eff m. MonadAff ( CustomEff eff ) m => Component m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = RT.Market

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = HH.div_
      [ HH.slot' CP.cp1 unit Navigation.component unit absurd
      , renderContent state
      , HH.slot' CP.cp4 unit Footer.component unit absurd
      ]

    renderContent :: RT.Routes -> H.ParentHTML Query ChildQuery ChildSlot m
    renderContent route = case route of
      RT.Crypto -> HH.h1_ [ HH.text "Crypto Currencies coming soon" ]
      RT.Forex -> HH.h1_ [ HH.text "Forex coming soon" ]
      RT.Market -> HH.slot' CP.cp2 unit Market.component unit absurd
      RT.Stock -> HH.slot' CP.cp3 unit Stock.component unit absurd

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output m
    eval = case _ of
      GOTO route next -> do
        H.liftAff $ log $ "Route >>>>>> " <> show route
        H.put route
        pure next

matchRoutes :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
                        -> Eff (HA.HalogenEffects eff) (Eff (HA.HalogenEffects eff) Unit)
matchRoutes app = matches RT.routing (\old new -> redirects app old new)
  where
    redirects driver _old = launchAff_ <<< driver.query <<< H.action <<< GOTO
