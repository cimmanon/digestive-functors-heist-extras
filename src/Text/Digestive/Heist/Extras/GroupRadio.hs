{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.GroupRadio
	( dfGroupRadioText
	, dfGroupRadioChoice
	) where

import Data.Function (on)
import Data.List (partition, unionBy)
import Data.Text (Text)
import Text.Digestive.Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.View

----------------------------------------------------------------------

appendElem :: Bool -> a -> [a] -> [a]
appendElem True x = (x:)
appendElem False _ = id

addAttrs :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
addAttrs = unionBy (on (==) fst)

----------------------------------------------------------------------

-- TODO: reduce code duplication here

dfGroupRadioText :: Monad m => View Text -> View Text -> Splice m
dfGroupRadioText outerView view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		(confAttrs, attrs') = partition ((== "checked") . fst) attrs
		value = fieldInputText ref view
		outerRef = case confAttrs of
			(("checked", x):_) -> x
			_ -> error "No field for comparison provided in `checked` attribute"
		checkedVal = fieldInputText outerRef outerView
		finalAttrs = addAttrs attrs' $
			appendElem (viewDisabled ref view) ("disabled", "") $
			appendElem (value == checkedVal) ("checked", "")
			[("type", "radio"), ("id", absoluteRef outerRef view), ("name", absoluteRef outerRef outerView), ("value", value)]
	return [X.Element "input" finalAttrs []]

dfGroupRadioChoice :: Monad m => View Text -> View Text -> Splice m
dfGroupRadioChoice outerView view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		(confAttrs, attrs') = partition ((== "checked") . fst) attrs
		value = fieldInputText ref view
		outerRef = case confAttrs of
			(("checked", x):_) -> x
			_ -> error "No field for comparison provided in `checked` attribute"
		selectedVals = map (\(x, _, _) -> x) $ filter (\(_, _, sel) -> sel) $ fieldInputChoice outerRef outerView
		finalAttrs = addAttrs attrs' $
			appendElem (viewDisabled ref view) ("disabled", "") $
			appendElem (value `elem` selectedVals) ("checked", "")
			[("type", "radio"), ("id", absoluteRef outerRef view), ("name", absoluteRef outerRef outerView), ("value", value)]
	return [X.Element "input" finalAttrs []]
