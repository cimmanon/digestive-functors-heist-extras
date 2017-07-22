{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras
	( module E

	, dfInputCheckboxMultiple
	) where

import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Text.Digestive.Heist
import Text.Digestive.View
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Heist.Extras.Plain as E
import Text.Digestive.Heist.Extras.Custom as E
import Text.Digestive.Heist.Extras.List as E
import Text.Digestive.Heist.Extras.GroupRadio as E

----------------------------------------------------------------------

-- this splice is intended for use with choiceMultiple forms.  it generates a
-- list of checkboxes, similar to dfInputRadio, but allows for some customization
{-# DEPRECATED dfInputCheckboxMultiple "Use Text.Digestive.Heist.Extras.Custom.dfCustomChoice instead" #-}
dfInputCheckboxMultiple :: Monad m => View T.Text -> Splice m
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
