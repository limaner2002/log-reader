module LogReader.Bootstrap where

import Prelude
import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.Markup as H
import qualified Text.Smolder.HTML.Attributes as A
import Data.Maybe
import Data.Array
import Data.Foldable
import Data.Monoid

class ToMarkup a where
    toMarkup :: a -> H.Markup

instance stringToMarkup :: ToMarkup String where
    toMarkup str = H.text str

instance maybeToMarkup :: (ToMarkup a) => ToMarkup (Maybe a) where
    toMarkup (Just val) = toMarkup val
    toMarkup Nothing = mempty

dataToggle = H.attribute "data-toggle"

data ComponentAccum = ComponentAccum H.Markup Int

buildComponent :: forall f a. (ToMarkup a, Foldable f)
               => (a -> Int -> H.Markup)
               -> f a
               -> H.MarkupM Unit
buildComponent createComponent items =
    getComponent componentAccum
  where
    accumComponent (ComponentAccum components oldIdx) item = ComponentAccum
                                                             (components <> createComponent item (oldIdx)) (oldIdx + 1)
    componentAccum = foldl accumComponent (ComponentAccum mempty 1) items
    getComponent (ComponentAccum componentMarkup _) = componentMarkup

data NavBar a b = NavBar
    { title :: a
    , content :: b
    }

data Pill a = Pill a Boolean Int
data Pills a = Pills (Array (Pill a))

instance pillToMarkup :: (ToMarkup a) => ToMarkup (Pill a) where
    toMarkup (Pill item isActive idx) = H.with (H.li (H.with (H.a $ toMarkup item) (A.href ("#item" <> show idx) <> dataToggle "tab"))) (markActive isActive)

instance pillsToMarkup :: (ToMarkup a) => ToMarkup (Pills a) where
    toMarkup pills =
        H.with (H.ul $ renderPills pills) (A.className "nav nav-pills nav-stacked")

markActive :: Boolean -> H.Attribute
markActive isActive =
    if isActive
    then A.className "active"
    else mempty

renderPills :: forall a. (ToMarkup a) => Pills a -> H.Markup
renderPills (Pills items) = buildComponent renderPill items
    where
      renderPill pill idx = toMarkup pill

buildVerticalNav :: forall a b. (ToMarkup a, ToMarkup b) => NavBar a b -> H.MarkupM Unit
buildVerticalNav (NavBar o) =
    H.with (H.div (titleContainer <> contentContainer)) (A.className "row")
  where
    titleContainer = H.with (H.div $ toMarkup o.title) (A.className "col-xs-3")
    contentContainer = H.with (H.div $ toMarkup o.content) (A.className "col-xs-9")

