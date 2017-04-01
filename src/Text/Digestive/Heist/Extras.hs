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
-- instead of creating a div, it creates a fieldset.  if the dfListItem attribute splice
-- is applied to a fieldset, the template will get disabled thanks to the disabled attribute.
-- this allows you to use the `required` attribute on form fields.
dfInputListCustom :: MonadIO m => (View T.Text -> Splices (Splice m)) -> View T.Text -> Splice m
dfInputListCustom splices view = do
	(ref, _) <- getRefAttributes Nothing
	let
		listRef = absoluteRef ref view
		listAttrs =
			[ ("id", listRef)
			, ("class", "inputList")
			]
			{-
		addControl _ = return $ disableOnclick ref view
			[ ("onclick", T.concat [ "addInputListItem(this, '"
								   , listRef
								   , "'); return false;"] ) ]
		removeControl _ = return $ disableOnclick ref view
			[ ("onclick", T.concat [ "removeInputListItem(this, '"
								   , listRef
								   , "'); return false;"] ) ]
								   -}
		itemAttrs v _ = return
			[ ("id", T.concat [listRef, ".", last $ "0" : viewContext v])
			, ("class", T.append listRef ".inputListItem inputListItem")
			]
		templateAttrs v _ = return
			[ ("id", T.concat [listRef, ".", last $ "-1" : viewContext v])
			, ("class", T.append listRef ".inputListTemplate inputListTemplate")
			, ("style", "display: none;")
			, ("disabled", "disabled")
			]
		items = listSubViews ref view
		f attrs v = localHS (bindAttributeSplices ("itemAttrs" ## attrs v) .
					bindDigestiveSplices' splices v) runChildren
		dfListItem = do
			template <- f templateAttrs (makeListSubView ref (-1) view)
			res <- mapSplices (f itemAttrs) items
			return $ template ++ res
			{-
		attrSplices = do
			"addControl"    ## addControl
			"removeControl" ## removeControl
			-}
		attrSplices = mempty
	nodes <- localHS (bindSplices ("dfListItem" ## dfListItem) .
					bindAttributeSplices attrSplices) runChildren
	let indices =
		[ X.Element "input"
			[ ("type", "hidden")
			, ("name", T.intercalate "." [listRef, indicesRef])
			, ("value", T.intercalate "," $ map
				(last . ("0":) . viewContext) items)
			] []
		]
	return [X.Element "fieldset" listAttrs (nodes ++ indices)]
