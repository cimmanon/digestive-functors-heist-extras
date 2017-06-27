{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras
	( dfPlainText
	, dfPlainChoice
--	, dfPlainChoiceGroup
	, dfPlainBool
--	, dfPlainFile
	, dfInputCheckboxMultiple
	, dfInputListStatic
	, dfInputListCustom
	) where

import Control.Monad.Trans
import Data.Map.Syntax (MapSyntax(..), (##))
import qualified Data.Text as T
import Text.Digestive.Heist
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X
import Data.Monoid (mempty)

import Text.Digestive.Form
import Text.Digestive.Form.List
import Text.Digestive.View

{----------------------------------------------------------------------------------------------------{
                                                                      | Plain Text
}----------------------------------------------------------------------------------------------------}

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
	mapSplices (textSplice . T.pack) $ fieldInputFile ref view

{----------------------------------------------------------------------------------------------------{
                                                                      | Choice
}----------------------------------------------------------------------------------------------------}

-- this splice is intended for use with choiceMultiple forms.  it generates a
-- list of checkboxes, similar to dfInputRadio, but allows for some customization
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

{----------------------------------------------------------------------------------------------------{
                                                                      | Form Lists
}----------------------------------------------------------------------------------------------------}

-- this is an extremely condensed version of dfInputList that only generates the list items,
-- does not generate the indices input element or additional markup
dfInputListStatic :: MonadIO m => (View T.Text -> Splices (Splice m)) -> View T.Text -> Splice m
dfInputListStatic splices view = do
	(ref, _) <- getRefAttributes Nothing
	let
		listRef = absoluteRef ref view
		items = listSubViews ref view
	runChildrenWith $ "dfListItem" ## mapSplices (runChildrenWith . digestiveSplices' splices) items

-- this is a variation on the dfInputList splice found in Text.Digestive.Heist
-- that does not generate any extra markup.  Instead, multiple splices and
-- attribute splices are available for recreating it using the exact markup
-- you want.
--
-- If you need to recreate the add/remove controls attributes,
-- use the dfListPath and dfListItemPath splices.

{-
Attribute Splices:
	listAttrs (intended for the list's container element; eg. div, fieldset)

Splices:
	indices
	dfListItem
		Attribute Splices:
			wrapperAttrs (intended for container elements; eg. li, tr)
			itemAttrs (intended for form elements; eg. input, fieldset, textarea)
			isDisabled
			isHidden

		Splices
			dfListItemPath (contains the path to the list item; eg. form.list_name.0)
			dfListItemType (indicates the item type; eg. inputListTemplate or inputListItem)
			dfIfInputListItem (show content if it is an inputListItem)
			dfIfInputListTemplate (show content if it is an inputListTemplate)
	dfListPath (contains the path to the list; eg. form.list_name)
-}
dfInputListCustom :: MonadIO m => (View T.Text -> Splices (Splice m)) -> View T.Text -> Splice m
dfInputListCustom splices view = do
	(ref, _) <- getRefAttributes Nothing
	let
		listRef = absoluteRef ref view
		listAttrs _ = return
			[ ("id", listRef)
			, ("class", "inputList")
			]

		templateAttrs v = do
			let
				itemRef = absoluteRef "" v
			"wrapperAttrs" ## \_ -> return
				[ ("id", itemRef)
				, ("class", T.append itemRef ".inputListTemplate inputListTemplate")
				, ("style", "display: none") ]
			"itemAttrs" ## \_ -> return
				[ ("style", "display: none")
				, ("disabled", "disabled")
				]
			"isDisabled" ## const (return [ ("disabled", "disabled") ])
			"isHidden" ## const (return [ ("style", "display: none") ])

		itemAttrs v = do
			let
				itemRef = absoluteRef "" v
			"wrapperAttrs" ## \_ -> return
				[ ("id", itemRef)
				, ("class", T.append itemRef ".inputListItem inputListItem")
				]
			"itemAttrs" ## const mempty
			"isDisabled" ## const mempty
			"isHidden" ## const mempty

		items = listSubViews ref view

		f itemType v = localHS (bindAttributeSplices (attrs v)) $ runChildrenWith $ do
			digestiveSplices' splices v
			"dfListItemPath" ## return [X.TextNode $ absoluteRef "" v]
			"dfListItemType" ## return [X.TextNode itemType]
			"dfIfInputListItem" ## if isInputListItem then runChildren else return []
			"dfIfInputListTemplate" ## if isInputListItem then return [] else runChildren
			where
				isInputListItem = itemType == "inputListItem"
				attrs = if isInputListItem then itemAttrs else templateAttrs

		dfListItem = do
			template <- f "inputListTemplate" (makeListSubView ref (-1) view)
			res <- mapSplices (f "inputListItem") items
			return $ template ++ res

		attrSplices = "listAttrs" ## listAttrs

		indices =
			[ X.Element "input"
				[ ("type", "hidden")
				, ("name", T.intercalate "." [listRef, indicesRef])
				, ("value", T.intercalate "," $ map
					(last . ("0":) . viewContext) items)
				] []
			]

	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"indices" ## return indices
		"dfListItem" ## dfListItem
		"dfListPath" ## return [X.TextNode listRef]
