module LogReader.Bootstrap where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as A
import qualified Halogen.Themes.Bootstrap3 as B

import Data.Array
import Data.Foldable (intercalate)
import Data.Monoid (mempty)
import Data.Maybe
import Data.Tuple

import Optic.Lens
import Optic.Getter
import Optic.Setter hiding (set)

data NavLink = NavLink
    { text :: String
    , active :: Boolean
    , target :: String
    }

instance navLinkShow :: Show NavLink where
    show (NavLink o) =
           "text: " <> o.text
        <> ", active: " <> show o.active
        <> ", target: " <> o.target

data Nav = Nav
    { items :: Array NavLink
    , navType :: NavType
    }

instance navShow :: Show Nav where
    show (Nav o) =
           "Nav{"
        <> " items: " <> show o.items
        <> " navType: " <> show o.navType

data NavType = Normal
             | Pill

instance navTypeShow :: Show NavType where
    show Normal = "Normal"
    show Pill = "Pill"

navbar :: forall p i. Nav -> H.HTML p i
navbar (Nav conf) =
    H.ul [ A.classes [B.nav, B.navPills, B.navStacked] ]
         (map renderNavItem conf.items)

    where
      renderNavItem (NavLink navLink) = H.li
                              (if navLink.active then [A.class_ B.active] else [])
                              [H.a [A.target navLink.target] [H.text navLink.text] ]


test = Nav { items: [ NavLink {text: "first", active: true, target: "fst"}
                    , NavLink {text: "second", active: false, target: "snd"}
                    ]
           , navType: Pill
           }

_active :: forall f. (Functor f) => (Boolean -> f Boolean) -> NavLink -> f NavLink
_active = lens (\(NavLink o) -> o.active)
               (\(NavLink o) active ->
                    NavLink { text: o.text
                            , active: active
                            , target: o.target
                            }
               )

_items = lens (\(Nav o) -> o.items)
              (\(Nav o) items ->
                   Nav { navType: o.navType
                       , items: items
                       }
              )

getActive :: Int -> Array NavLink -> Maybe Boolean
getActive idx arr =
    map (^. _active) $ arr !! idx


setActive :: Int -> Nav -> Nav
setActive idx nav =
   (_items .~ setActive') nav
  where
    setActive' = map setIndexed indexed
    indexed = zip indices items
    indices = 0 .. ((length items) - 1)
    items = nav ^. _items
    setIndexed (Tuple i item)
        | i == idx = (_active .~ true) item
        | otherwise = (_active .~ false) item
