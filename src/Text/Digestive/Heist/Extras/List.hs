{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.List
	( dfInputListStatic
	, dfInputListCustom
	, dfInputListSpan
	) where

import Data.Map.Syntax ((##))
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import qualified Text.XmlHtml as X

import Text.Digestive.Form.List
import Text.Digestive.View

import Text.Digestive.Heist.Extras.Conditional (dfIfDisabled, dfIfEnabled)
import Text.Digestive.Heist.Extras.Internal.Attribute (getRefAttributes, appendAttr)
import Text.Digestive.Heist.Extras.Internal.View (disableView)

----------------------------------------------------------------------

-- this is an extremely condensed version of dfInputList that only generates the list items,
-- does not generate the indices input element or additional markup.  it will also remove
-- the child nodes if the list is empty
dfInputListStatic :: Monad m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
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
			dfEditEnabled (similar to dfIfEnabled, but is always enabled for templates)
			dfEditDisabled (similar to dfIfDisabled, but is always disabled for templates)
	dfListPath (contains the path to the list; eg. form.list_name)
-}
dfInputListCustom :: Monad m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
dfInputListCustom splices view = do
	(ref, _) <- getRefAttributes Nothing
	let
		listRef = absoluteRef ref view
		listAttrs _ = return
			[ ("id", listRef)
			, ("class", "inputList")
			]

		items = listSubViews ref view

		attrSplices = "listAttrs" ## listAttrs

	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"indices" ## indicesSplice listRef items
		"dfListItem" ## onlyListItemsSplice splices (listTemplateView ref view) items
		"dfListPath" ## return [X.TextNode listRef]

-- A variation on dfInputListCustom that exects to work with a lit of forms
-- that contains a list of forms with the intent of having the outer list in
-- a table that spans the inner list.  Note that it requires 2 ref attributes.
-- Also note that you don't want to use a dfInputList* splice inside of it for
-- the inner list, that's automaticaly handled for you.  The inner list cannot
-- be dynamic (this is mostly due to a limitation with the JavaScript library).

{-
<dfInputListSpan group="outerlistref" ref="innerlistref"><div listAttrs>
	<indices/><!-- outer list indices -->

	<table>
		<thead>
			<th>Outer list content</th>
			<th>Inner list content</th>
			<th>Inner list indices</th>
		</thead>

		<dfListGroup><tbody wrapperAttrs>
			<dfListItem><tr>
				<dfGroupItem><td groupspan><!-- outer list content -->
					This cell spans all the rows
				</td></dfGroupItem>

				<td>This cell exists in each row</td><!-- inner list content -->
				<td><indices/></td><!-- inner list content -->
			</tr></dfListItem>
		</tbody></dfListGroup>
	</table>
</div></dfInputListSpan>
-}
dfInputListSpan :: Monad m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
dfInputListSpan splices view = do
	(groupRef, _) <- getRefAttributes $ Just "group"
	(itemRef, _) <- getRefAttributes Nothing
	let
		groupListRef = absoluteRef groupRef view
		listAttrs _ = return
			[ ("id", groupListRef)
			, ("class", "inputList inputGroup")
			]

		groupItems = listSubViews groupRef view

		-- this splice is for the individual items in groupItems
		groupItemSplices v = do
			let
				listRef = absoluteRef itemRef v
				items = listSubViews itemRef v
				totalItemsT = T.pack $ show $ length items

				headAttrSplices = "groupspan" ## const $ return [("rowspan", totalItemsT)]
				headSplice = localHS (bindAttributeSplices headAttrSplices) $ runChildrenWith $ do
					splices v
					"total_items" ## return [X.TextNode totalItemsT]
				tailSplice = return []

				splice s v' = runChildrenWith $ do
					splices v'
					"dfGroupItem" ## s

				itemsSplice = case items of
					(x:xs) -> do
						x' <- splice headSplice x
						xs' <- mapSplices (splice tailSplice) xs
						return $ x' ++ xs'
					_ -> mapSplices (splice headSplice) items

			--splices v
			"indices" ## indicesSplice listRef items
			"dfListItem" ## itemsSplice
			"dfListPath" ## return [X.TextNode listRef]

		attrSplices = "listAttrs" ## listAttrs

	localHS (bindAttributeSplices attrSplices) $ runChildrenWith $ do
		"indices" ## indicesSplice groupListRef groupItems
		"dfListGroup" ## onlyListItemsSplice groupItemSplices (listTemplateView groupRef view) groupItems
		"dfListGroupPath" ## return [X.TextNode groupListRef]

{----------------------------------------------------------------------------------------------------{
                                                                      | Common Helper Functions
}----------------------------------------------------------------------------------------------------}

indicesSplice :: Monad m => Text -> [View v] -> Splice m
indicesSplice listRef items =
	return $ [ X.Element "input"
		[ ("type", "hidden")
		, ("name", T.intercalate "." [listRef, indicesRef])
		, ("value", T.intercalate "," $ map
			(last . ("0":) . viewContext) items)
		] []
	]

-- this splice looks for an "only" attribute that will let you pick
-- between only the template or only the data, omitting the attribute
-- will display both
onlyListItemsSplice :: Monad m => (View Text -> Splices (Splice m)) -> View Text -> [View Text] -> Splice m
onlyListItemsSplice splices template items = do
	node <- getParamNode
	let
		listTemplate = listItemSplice splices True $ disableView template
		listItems = mapSplices (listItemSplice splices False) items
	case X.getAttribute "only" node of
		Just "template" -> listTemplate
		Just _ -> listItems
		Nothing -> do
			t <- listTemplate
			xs <- listItems
			return $ t ++ xs

listTemplateView :: Text -> View Text -> View Text
listTemplateView ref view = makeListSubView ref (-1) view

listItemSplice :: Monad m => (View Text -> Splices (Splice m)) -> Bool -> View Text -> Splice m
listItemSplice splices isTemplate v = localHS (bindAttributeSplices (listItemAttrs isTemplate v)) $ runChildrenWith $ do
	splices v
	"dfListItemIndex" ## return [X.TextNode $ last $ T.split (== '.') $ absoluteRef "" v]
	"dfListItemPath" ## return [X.TextNode $ absoluteRef "" v]
	"dfListItemType" ## return [X.TextNode $ ifElseTemplate "inputListTemplate" "inputListItem"]
	"dfIfInputListItem" ## ifElseTemplate (return []) runChildren
	"dfIfInputListTemplate" ## ifElseTemplate runChildren (return [])
	"dfEditEnabled" ## ifElseTemplate runChildren $ dfIfEnabled v
	"dfEditDisabled" ## ifElseTemplate (return []) $ dfIfDisabled v
	where
		ifElseTemplate forTemplate forListItem = if isTemplate then forTemplate else forListItem

--------------------------------------------------------------------- | Attribute Splices

listItemAttrs :: Monad m => Bool -> View Text -> Splices (AttrSplice m)
listItemAttrs isTemplate v = do
	let
		itemRef = absoluteRef "" v
	"wrapperAttrs" ## \_ -> return $ appendAttr isTemplate ("style", "display: none")
		[ ("id", itemRef)
		, ("class", T.append itemRef $ if isTemplate then ".inputListTemplate inputListTemplate" else ".inputListItem inputListItem")
		]
	"itemAttrs" ## \_ -> return $ case isTemplate of
		True ->
			[ ("style", "display: none")
			, ("disabled", "disabled")
			]
		False -> []
	"isDisabled" ## const (return $ appendAttr isTemplate ("disabled", "disabled") [])
	"isHidden" ## const (return $ appendAttr isTemplate ("style", "display: none") [])
