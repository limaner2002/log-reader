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

data Pills a = Pills (Array a)

instance pillsToMarkup :: (ToMarkup a) => ToMarkup (Pills a) where
    toMarkup (Pills items) =
        H.with (H.ul $ pills items) (A.className "nav nav-pills nav-stacked")

pills :: forall f a. (Foldable f, ToMarkup a) => f a -> H.Markup
pills items = buildComponent createPill items
    where
      createPill item idx =
          H.with (H.li (H.with (H.a $ toMarkup item) (A.href ("#item" <> show idx) <> dataToggle "tab"))) (isActive idx)
      isActive :: Int -> H.Attribute
      isActive idx =
          if idx == 1
          then A.className "active"
          else mempty

buildVerticalNav :: forall a b. (ToMarkup a, ToMarkup b) => NavBar a b -> H.MarkupM Unit
buildVerticalNav (NavBar o) =
    H.with (H.div (titleContainer <> contentContainer)) (A.className "row")
  where
    titleContainer = H.with (H.div $ toMarkup o.title) (A.className "col-xs-3")
    contentContainer = H.with (H.div $ toMarkup o.content) (A.className "col-xs-9")