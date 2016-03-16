module LogReader.UI where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

type LogFile = { fileName :: Text }

ui :: forall g. (Functor g) => Component LogFile Query g
ui = componenent render eval
    where
      render :: LogFile -> ComponentHTML Query
      render name =
          H.div_