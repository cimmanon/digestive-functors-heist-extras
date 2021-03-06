{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Digestive.Heist.Extras.Custom
	( dfCustomText
	, dfCustomChoice
	, dfCustomChoiceGroup
	, dfCustomBool
	, dfCustomFile
	) where

import Control.Exception (SomeException, try)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map.Syntax ((##))
import qualified Data.ByteString.Base64 as B64 -- experimental
import qualified Data.ByteString as BS -- experimental
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as BS -- experimental
import Text.Digestive.View
import Heist
import Heist.Interpreted

import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes, disabledAttrSplice, checkedAttrSplice)
import Text.Digestive.Heist.Extras.Internal.Field (selectedChoiceValues, selectedChoiceGroupValues)

----------------------------------------------------------------------

-- these splice allow you to create markup exactly the way you want

dfCustomText :: Monad m => View Text -> Splice m
dfCustomText view = do
	(ref, _) <- getRefAttributes Nothing
	let
		attrSplices = "isDisabled" ## disabledAttrSplice $ viewDisabled ref view
	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"path" ## textSplice $ absoluteRef ref view
		"value" ## textSplice $ fieldInputText ref view

dfCustomChoice :: Monad m => View Text -> Splice m
dfCustomChoice view = do
	(ref, _) <- getRefAttributes Nothing
	let
		choices = fieldInputChoice ref view
		selected = selectedChoiceValues choices
		attrSplices = "isDisabled" ## disabledAttrSplice $ viewDisabled ref view
	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"path" ## textSplice $ absoluteRef ref view
		"choice" ## mapSplices choiceSplice choices
		"selected" ## mapSplices choiceSplice selected

dfCustomChoiceGroup :: Monad m => View Text -> Splice m
dfCustomChoiceGroup view = do
	(ref, _) <- getRefAttributes Nothing
	let
		choices = fieldInputChoiceGroup ref view
		selected = selectedChoiceGroupValues choices
		attrSplices = "isDisabled" ## disabledAttrSplice $ viewDisabled ref view
	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"path" ## textSplice $ absoluteRef ref view
		"choice" ## mapSplices groupSplice choices
		"selected" ## mapSplices groupSplice selected
	where
		groupSplice (name, options) = runChildrenWith $ do
			"group" ## textSplice name
			"choice" ## mapSplices choiceSplice options

dfCustomBool :: Monad m => View Text -> Splice m
dfCustomBool view = do
	(ref, _) <- getRefAttributes Nothing
	let
		checked = fieldInputBool ref view
		attrSplices = do
			"isDisabled" ## disabledAttrSplice $ viewDisabled ref view
			"isChecked" ## checkedAttrSplice checked
	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"path" ## textSplice $ absoluteRef ref view
		"value" ## textSplice $ if checked then "Yes" else "No"

dfCustomFile :: MonadIO m => View Text -> Splice m
dfCustomFile view = do
	(ref, _) <- getRefAttributes Nothing
	let
		attrSplices = "isDisabled" ## disabledAttrSplice $ viewDisabled ref view
	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"path" ## textSplice $ absoluteRef ref view
		"file" ## mapSplices fileSplice $ fieldInputFile ref view
	where
		fileSplice f = do
			d <- dataSplice f
			runChildrenWith $ do
				"name" ## textSplice $ T.pack f
				"data" ## return d
				"IfEmpty" ## if null d then runChildren else return []
				"IfNotEmpty" ## if null d then return [] else runChildren

--------------------------------------------------------------------- | Common Splices

choiceSplice :: Monad m => (Text, Text, Bool) -> Splice m
choiceSplice (val, name, sel) =
	let
		attrSplices = do
			"isChecked" ## checkedAttrSplice sel
		splices = do
			"name" ## textSplice name
			"value" ## textSplice val
	in
		localHS (bindAttributeSplices attrSplices) $ runChildrenWith splices

dataSplice :: MonadIO m => FilePath -> Splice m
dataSplice path = do
	contents :: (Either SomeException BS.ByteString) <- liftIO $ try (BS.readFile path)
	case contents of
		Right c | c /= "" -> textSplice $ BS.decodeUtf8 $ B64.encode c
		_ -> return []
