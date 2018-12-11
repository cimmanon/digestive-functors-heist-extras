{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Patch
	( dfSubView
	, dfInputSelectGroup
	) where

import Data.Text (Text)
import Text.Digestive.View (View, absoluteRef, subView, fieldInputChoiceGroup, viewDisabled)
import Heist (Splices)
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes, appendAttr, mergeAttrs, disabledAttr)
import Text.Digestive.Heist.Extras.Internal.Splice (AppendableSplice, runSplices)

-- This module contains rewrites of splices found in the standard library.

----------------------------------------------------------------------
-- variation of Text.Digestive.Heist.dfSubView that allows you to pass in a
-- list of splices to be made accessible in the child nodes
dfSubView :: Monad m => AppendableSplice m -> View Text -> Splice m
dfSubView splices view = do
	(ref, _) <- getRefAttributes Nothing
	runChildrenWith $ runSplices splices $ subView ref view

----------------------------------------------------------------------

-- variation of Text.Digestive.Heist.dfSubView.dfInputSelectGroup, gracefully
-- handles `multiple` option for optional results
dfInputSelectGroup :: Monad m => View Text -> Splice m
dfInputSelectGroup view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		ref'     = absoluteRef ref view
		choices  = fieldInputChoiceGroup ref view
		kids     = concatMap makeGroup choices
		finalAttrs = mergeAttrs attrs $ appendAttr (viewDisabled ref view) disabledAttr [("id", ref'), ("name", ref')]

		makeGroup ("", options) = map makeOption options
		makeGroup (name, options) = [X.Element "optgroup" [("label", name)] $ map makeOption options]

		makeOption (i, c, sel) = X.Element "option" (appendAttr sel ("selected", "selected") [("value", i)]) [X.TextNode c]

	return [X.Element "select" finalAttrs kids]
