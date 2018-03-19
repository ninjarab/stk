module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Typeahead.Container (component)

main :: Eff (HA.HalogenEffects ( ajax :: AX.AJAX, console :: CONSOLE, dom :: DOM, now :: NOW, avar :: AVAR, timer :: TIMER )) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
