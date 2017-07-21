{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Plain
	( dfPlainText
	, dfPlainChoice
	, dfPlainChoiceGroup
	, dfPlainBool
	, dfPlainFile
	) where

import Control.Monad.Trans
import Data.Map.Syntax (MapSyntax(..), (##))
import qualified Data.Text as T
import Text.Digestive.Heist
import Text.Digestive.View
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X

----------------------------------------------------------------------

-- these splices allow us to grab the values out of a given type of Field.
dfPlainText :: Monad m => View v -> Splice m
dfPlainText view = do
	(ref, _) <- getRefAttributes Nothing
	textSplice $ fieldInputText ref view

dfPlainChoice :: Monad m => View T.Text -> Splice m
dfPlainChoice view = do
	(ref, _) <- getRefAttributes Nothing
	let
		vals = filter (\(_, _, sel) -> sel) $ fieldInputChoice ref view
		choiceSplice (val, name, _) = runChildrenWith $ do
			"value" ## textSplice val
			"name" ## textSplice name
	mapSplices choiceSplice vals

dfPlainChoiceGroup :: Monad m => View T.Text -> Splice m
dfPlainChoiceGroup view = do
	(ref, _) <- getRefAttributes Nothing
	let
		filterChoice (_, _, sel) = sel
		vals = filter (\(_, options) -> not $ null $ filter filterChoice options) $ fieldInputChoiceGroup ref view
		groupSplice (name, options) = runChildrenWith $ do
			"group" ## textSplice name
			"choice" ## mapSplices choiceSplice $ filter filterChoice options
		choiceSplice (val, name, _) = runChildrenWith $ do
			"value" ## textSplice val
			"name" ## textSplice name
	mapSplices groupSplice vals

dfPlainBool :: Monad m => View T.Text -> Splice m
dfPlainBool view = do
	(ref, _) <- getRefAttributes Nothing
	textSplice $ if (fieldInputBool ref view) then "Yes" else "No"

dfPlainFile :: Monad m => View T.Text -> Splice m
dfPlainFile view = do
	(ref, _) <- getRefAttributes Nothing
	mapSplices (\x -> runChildrenWith $ "name" ## textSplice $ T.pack x) $ fieldInputFile ref view
