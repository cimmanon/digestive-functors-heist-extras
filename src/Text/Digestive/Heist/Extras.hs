{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras
	( module E

	, dfPath
	, dfInputCheckboxMultiple
	) where

import Data.Map.Syntax ((##))
import Data.Text (Text)
import Text.Digestive.View (View, absoluteRef, fieldInputChoice)
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Heist.Extras.Plain as E
import Text.Digestive.Heist.Extras.Custom as E
import Text.Digestive.Heist.Extras.List as E (dfInputListStatic, dfInputListCustom, dfInputListSpan)
import Text.Digestive.Heist.Extras.GroupRadio as E
import Text.Digestive.Heist.Extras.Patch as E
import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)

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
