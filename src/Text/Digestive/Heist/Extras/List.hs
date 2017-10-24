{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.List
	( dfInputListStatic
	, dfInputListCustom
	) where

import Control.Monad.Trans (MonadIO)
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X
import Data.Monoid (mempty)

import Text.Digestive.Form.List
import Text.Digestive.View

import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes)

----------------------------------------------------------------------

-- this is an extremely condensed version of dfInputList that only generates the list items,
-- does not generate the indices input element or additional markup.  it will also remove
-- the child nodes if the list is empty
dfInputListStatic :: MonadIO m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
dfInputListStatic splices view = do
	(ref, _) <- getRefAttributes Nothing
	let
		items = listSubViews ref view
	case items of
		[] -> return []
		_ -> runChildrenWith $ "dfListItem" ## mapSplices (runChildrenWith . splices) items

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
			dfListItemIndex (contains the index value for the list item; eg. 0)
			dfListItemPath (contains the path to the list item; eg. form.list_name.0)
			dfListItemType (indicates the item type; eg. inputListTemplate or inputListItem)
			dfIfInputListItem (show content if it is an inputListItem)
			dfIfInputListTemplate (show content if it is an inputListTemplate)
	dfListPath (contains the path to the list; eg. form.list_name)
-}
dfInputListCustom :: MonadIO m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
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
			splices v
			"dfListItemIndex" ## return [X.TextNode $ last $ T.split (== '.') $ absoluteRef "" v]
			"dfListItemPath" ## return [X.TextNode $ absoluteRef "" v]
			"dfListItemType" ## return [X.TextNode itemType]
			"dfIfInputListItem" ## if isInputListItem then runChildren else return []
			"dfIfInputListTemplate" ## if isInputListItem then return [] else runChildren
			where
				isInputListItem = itemType == "inputListItem"
				attrs = if isInputListItem then itemAttrs else templateAttrs

		-- this splice looks for an "only" attribute that will let you pick
		-- between only the template or only the data, omitting the attribute
		-- will display both
		dfListItem = do
			node <- getParamNode
			let
				listTemplate = f "inputListTemplate" (makeListSubView ref (-1) view)
				listItems = mapSplices (f "inputListItem") items
			case X.getAttribute "only" node of
				Just "template" -> listTemplate
				Just _ -> listItems
				Nothing -> do
					t <- listTemplate
					xs <- listItems
					return $ t ++ xs

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
