{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Conditional
	( dfIfText
	) where

import Data.Map.Syntax ((##))
import Data.List (find)
--import qualified Data.Text as T
--import Data.Text (Text)
import Text.Digestive.Heist
import Text.Digestive.View
import Heist.Interpreted

----------------------------------------------------------------------
-- Splices that allow us to hide content if the referenced value is empty

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
