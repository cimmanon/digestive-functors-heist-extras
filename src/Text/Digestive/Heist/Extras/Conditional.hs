{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Conditional
	( dfIfText
	, dfIfNotText
	, dfIfTrue
	, dfIfFalse

	, dfIfChoice
	, dfIfNotChoice

	, dfIfDisabled
	, dfIfEnabled
	) where

import Data.Map.Syntax ((##))
import Data.List (find)
--import qualified Data.Text as T
import Data.Text (Text)
import Text.Digestive.View
import Heist.Interpreted

import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)
import Text.Digestive.Heist.Extras.Internal.Field (selectedChoiceValues)

{----------------------------------------------------------------------------------------------------{
                                                                      | Visibility based on a value
}----------------------------------------------------------------------------------------------------}

-- Text
-- Hide content if the referenced value is empty
-- Or only show content if the value is equal to a specific string
dfIfText :: Monad m => View v -> Splice m
dfIfText view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		val = fieldInputText ref view
		splice = runChildrenWith $ do
			"path" ## textSplice $ absoluteRef ref view
			"value" ## textSplice val
	case find ((== "equals") . fst) attrs of
		Just (_, v) | val == v -> splice
		-- ^ if we find the `equals` attribute, check if the provided value is equal to the ref value
		Nothing | val /= "" -> splice
		-- ^ if there's no `equals` attribute, display the contents if the ref value is not empty
		_ -> return []
		-- ^ otherwise hide the contents

dfIfNotText :: Monad m => View v -> Splice m
dfIfNotText view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		val = fieldInputText ref view
		splice = runChildrenWith $ do
			"path" ## textSplice $ absoluteRef ref view
			"value" ## textSplice val
	case find ((== "equals") . fst) attrs of
		Just (_, v) | val /= v -> splice
		-- ^ if we find the `equals` attribute, check if the provided value is not equal to the ref value
		Nothing | val == "" -> splice
		-- ^ if there's no `equals` attribute, display the contents if the ref value is empty
		_ -> return []
		-- ^ otherwise hide the contents

-- Choice
dfIfChoice :: Monad m => View Text -> Splice m
dfIfChoice view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		vals = fieldInputChoice ref view
		selectedVals = map (\(x, _,  _) -> x) $ selectedChoiceValues vals
		choiceSplice (val, name, _) = runChildrenWith $ do
			"value" ## textSplice val
			"name" ## textSplice name
	case find ((== "equals") . fst) attrs of
		Just (_, v) | v `elem` selectedVals -> runChildren
		-- ^ if we find the `equals` attribute, check if the provided value is not equal to the ref value
		_ -> return []
		-- ^ otherwise hide the contents

dfIfNotChoice :: Monad m => View Text -> Splice m
dfIfNotChoice view = do
	(ref, attrs) <- getRefAttributes Nothing
	let
		vals = fieldInputChoice ref view
		selectedVals = map (\(x, _,  _) -> x) $ selectedChoiceValues vals
		choiceSplice (val, name, _) = runChildrenWith $ do
			"value" ## textSplice val
			"name" ## textSplice name
	case find ((== "equals") . fst) attrs of
		Just (_, v) | v `notElem` selectedVals -> runChildren
		-- ^ if we find the `equals` attribute, check if the provided value is not equal to the ref value
		_ -> return []
		-- ^ otherwise hide the contents

-- Bool
dfIfTrue :: Monad m => View v -> Splice m
dfIfTrue view = do
	(ref, _) <- getRefAttributes Nothing
	if fieldInputBool ref view then runChildren else return []

dfIfFalse :: Monad m => View v -> Splice m
dfIfFalse view = do
	(ref, _) <- getRefAttributes Nothing
	if fieldInputBool ref view then return [] else runChildren

{----------------------------------------------------------------------------------------------------{
                                                                      | Visibility based on disabled status
}----------------------------------------------------------------------------------------------------}

dfIfDisabled :: Monad m => View v -> Splice m
dfIfDisabled view = do
	(ref, _) <- getRefAttributes Nothing
	if viewDisabled ref view
		then runChildren
		else return []

dfIfEnabled :: Monad m => View v -> Splice m
dfIfEnabled view = do
	(ref, _) <- getRefAttributes Nothing
	if viewDisabled ref view
		then return []
		else runChildren
