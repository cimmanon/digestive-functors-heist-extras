{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.GroupRadio
	( dfGroupRadioText
	, dfGroupRadioChoice
	) where

import Data.Text (Text)
import Text.Digestive.View
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Heist.Extras.Internal.Attribute

----------------------------------------------------------------------

-- TODO: reduce code duplication here

dfGroupRadioText :: Monad m => View Text -> View Text -> Splice m
dfGroupRadioText outerView view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		(outerRef, attrs') = extractAttr "checked" attrs
		value = fieldInputText ref view
		checkedVal = fieldInputText outerRef outerView
		finalAttrs = mergeAttrs attrs' $
			appendAttr (viewDisabled ref view) disabledAttr $
			appendAttr (value == checkedVal) checkedAttr
			[("type", "radio"), ("id", absoluteRef outerRef view), ("name", absoluteRef outerRef outerView), ("value", value)]
	return [X.Element "input" finalAttrs []]

dfGroupRadioChoice :: Monad m => View Text -> View Text -> Splice m
dfGroupRadioChoice outerView view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		(outerRef, attrs') = extractAttr "checked" attrs
		value = fieldInputText ref view
		selectedVals = map (\(x, _, _) -> x) $ filter (\(_, _, sel) -> sel) $ fieldInputChoice outerRef outerView
		finalAttrs = mergeAttrs attrs' $
			appendAttr (viewDisabled ref view) disabledAttr $
			appendAttr (value `elem` selectedVals) checkedAttr
			[("type", "radio"), ("id", absoluteRef outerRef view), ("name", absoluteRef outerRef outerView), ("value", value)]
	return [X.Element "input" finalAttrs []]
