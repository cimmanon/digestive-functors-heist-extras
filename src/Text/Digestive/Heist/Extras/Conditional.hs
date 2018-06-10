{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Conditional
	( dfIfText
	, dfIfTrue
	, dfIfFalse

	, dfIfDisabled
	, dfIfEnabled
	, dfIfNotDisabled
	) where

import Data.Map.Syntax ((##))
import Data.List (find)
--import qualified Data.Text as T
--import Data.Text (Text)
import Text.Digestive.View
import Heist.Interpreted

import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)

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

-- not sure which function name I'm going to prefer, so including them both
dfIfEnabled :: Monad m => View v -> Splice m
dfIfEnabled view = do
	(ref, _) <- getRefAttributes Nothing
	if viewDisabled ref view
		then return []
		else runChildren

dfIfNotDisabled :: Monad m => View v -> Splice m
dfIfNotDisabled = dfIfNotDisabled
