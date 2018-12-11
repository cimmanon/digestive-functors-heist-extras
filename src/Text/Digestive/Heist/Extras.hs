{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras
	( module E

	, AppendableSplice
	, digestiveSplices
	, digestiveSplicesWith
	, runSplices
	, addSplices
	, dfPath
	, dfInputCheckboxMultiple
	) where

import Data.Map.Syntax ((##))
import Control.Monad.Trans (MonadIO)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Text.Digestive.Heist as DF (digestiveSplices)
import Text.Digestive.View (View, absoluteRef, fieldInputChoice)
import Heist (Splices)
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Heist.Extras.Plain as E
import Text.Digestive.Heist.Extras.Custom as E
import Text.Digestive.Heist.Extras.List as E (dfInputListStatic, dfInputListCustom, dfInputListSpan)
import Text.Digestive.Heist.Extras.GroupRadio as E
import Text.Digestive.Heist.Extras.Patch as E
import Text.Digestive.Heist.Extras.Internal.Splice (AppendableSplice, runSplices, mergeSplices, addSplices)
import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)

----------------------------------------------------------------------

digestiveSplices :: (Monad m, MonadIO m) => View Text -> Splices (Splice m)
digestiveSplices = digestiveSplicesWith mempty

digestiveSplicesWith :: (Monad m, MonadIO m) => AppendableSplice m -> View Text -> Splices (Splice m)
digestiveSplicesWith moreSplices = (mergeSplices baseSplices moreSplices) mempty

baseSplices :: (Monad m, MonadIO m) => AppendableSplice m
baseSplices s v = do
	let
		nextSplices = addSplices baseSplices s
	DF.digestiveSplices v
	"dfPath" ## dfPath v
	"dfSubView" ## dfSubView nextSplices v
	"dfInputListStatic" ## dfInputListStatic nextSplices v
	"dfInputListCustom" ## dfInputListCustom nextSplices v

----------------------------------------------------------------------

-- basic text splice that provides the current path in the form,
-- handy when you have to manually create some form markup
-- (eg. form.subview)
dfPath :: Monad m => View Text -> Splice m
dfPath view = return [X.TextNode $ absoluteRef "" view]

----------------------------------------------------------------------

-- this splice is intended for use with choiceMultiple forms.  it generates a
-- list of checkboxes, similar to dfInputRadio, but allows for some customization
{-# DEPRECATED dfInputCheckboxMultiple "Use Text.Digestive.Heist.Extras.Custom.dfCustomChoice instead" #-}
dfInputCheckboxMultiple :: Monad m => View Text -> Splice m
dfInputCheckboxMultiple view =  do
	(ref, _) <- getRefAttributes Nothing
	let
		ref' = absoluteRef ref view
		choices = fieldInputChoice ref view
		--value i = ref' <> "." <> i

		checkboxSplice (i, c, sel) = do
			let
				defaultAttributes = [("type", "checkbox"), ("name", ref'), ("value", i)]
				attrs = if sel then ("checked", "checked") : defaultAttributes else defaultAttributes
			"checkbox" ## return [X.Element "input" attrs []]
			"name" ## return [X.TextNode c]

	mapSplices (runChildrenWith . checkboxSplice) choices
