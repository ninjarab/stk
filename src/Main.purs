module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)

import Halogen.Aff as HA
import Halogen.ECharts as EC
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax as AX

import Application (component, matchRoutes)

main :: Eff (HA.HalogenEffects ( EC.EChartsEffects ( console :: CONSOLE, ajax :: AX.AJAX, timer :: TIMER ) )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- runUI component unit body
  liftEff $ matchRoutes app
