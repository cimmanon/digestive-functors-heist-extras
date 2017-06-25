{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras
	( dfPlainText
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
                                                                      | Simple
}----------------------------------------------------------------------------------------------------}

dfPlainText :: Monad m => View v -> Splice m
dfPlainText view = do
	(ref, _) <- getRefAttributes Nothing
	return [X.TextNode $ fieldInputText ref view]

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

		f itemType attrs v = localHS (bindAttributeSplices (attrs v)) $ runChildrenWith $ do
			digestiveSplices' splices v
			"dfListItemPath" ## return [X.TextNode $ absoluteRef "" v]
			"dfListItemType" ## return [X.TextNode itemType]

		dfListItem = do
			template <- f "inputListTemplate" templateAttrs (makeListSubView ref (-1) view)
			res <- mapSplices (f "inputListItem" itemAttrs) items
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
