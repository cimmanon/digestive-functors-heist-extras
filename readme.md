This is a collection of custom splices for use with Digestive Functors Heist.  Some of the default splices generate invalid markup or don't play nicely with the newer HTML5 form attributes like `required`.

These are intended to be used with either `digestiveSplices'` or `bindDigestiveSplices'`, which can be found in the `custom-splices` branch of [digestive-functors](https://github.com/cimmanon/digestive-functors) for the digestive-functors-heist library.

# Using the splices

There's no bindings, you'll have to create your own before you can use them.

```haskell
digestiveSplicesCustom :: MonadIO m => View Text -> Splices (Splice m)
digestiveSplicesCustom = digestiveSplices' splices
	where
		splices v = do
			"dfPlainText" ## dfPlainText v
			"dfInputListCustom" ## dfInputListCustom digestiveSplicesCustom v

fooH :: AppHandler ()
fooH = do
	(view, result) <- runForm "form" fooForm
	renderWithSplices "path/to/template" $ digestiveSplicesCustom view
```

Alternately:

```haskell
digestiveSplicesCustom' :: MonadIO m => View Text -> Splices (Splice m)
digestiveSplicesCustom' v = do
	"dfPlainText" ## dfPlainText v
	"dfInputListCustom" ## dfInputListCustom digestiveSplicesCustom v

fooH' :: AppHandler ()
fooH' = do
	(view, result) <- runForm "form" fooForm
	renderWithSplices "path/to/template" $ digestiveSplices' digestiveSplicesCustom' view
```

## GroupRadio

The GroupRadio collection of splices is intended to allow a radio element to span a list of forms.  For instance, you might have a list of email addresses where only one of them is allowed to be the default.

In order to take advantage of the splice, your form needs to have a field that holds the value you want to match against.

```haskell
featuredForm :: Monad m => [a] -> Form Text m (Text, [a])
featuredForm xs = ( , )
	<$> "selected" .: text (getSelected xs)
	<*> "list" .: listOf aForm (Just xs)
	where
		aForm = undefined
		-- ^ form for the list
		getSelected = undefined
		-- ^ extract the selected element out of the list
```

Binding the GroupRadio collection of splices is not as straight forward as other splices because it requires two views:  the view from outside of the list and the view within the list item.

```haskell
digestiveSplicesCustom :: MonadIO m => View Text -> Splices (Splice m)
digestiveSplicesCustom = digestiveSplices' splices
	where
		splices v = do
			"dfPath" ## dfPath v
			"dfInputListCustom" ## dfInputListCustom' digestiveSplicesCustom v

dfInputListCustom' :: MonadIO m => (View Text -> Splices (Splice m)) -> View Text -> Splice m
dfInputListCustom' splices outerView =
	let
		listItemSplices listItemView = do
			splices listItemView
			"dfGroupRadioText" ## dfGroupRadioText outerView listItemView
	in
		dfInputListCustom listItemSplices outerView
```

Here's a template for a list of images where only one can be a chosen as a featured image.  Note the `checked="selected"` attribute on the dfGroupRadioText element, this tells the splice which field you want to compare it to in order to determine if it is selected or not.  This field must be visible from the outer view, which was the first argument passed into the splice when we bound it.

```haskell
<dfInputListCustom ref="list">
<table class="spreadsheet">
	<caption>Current Images</caption>

	<thead>
		<tr>
			<th>Preview</th>
			<th>Filename</th>
			<th>Width</th>
			<th>Height</th>
			<th>Featured</th>
			<th>Delete</th>
		</tr>
	</thead>

	<tbody>
		<dfListItem><tr wrapperAttrs>
			<td><dfCustomText ref="filename"><img src="/screenshots/${slug}/${value}" width="200" alt="" /></dfCustomText></td>
			<td><dfPlainText ref="filename">example.jpg</dfPlainText></td>
			<td><dfPlainText ref="width">480</dfPlainText>px</td>
			<td><dfPlainText ref="height">320</dfPlainText>px</td>
			<td><label><dfGroupRadioText ref="filename" checked="selected" /> Featured</label></td>
			<td><label><dfInputCheckbox ref="delete" /> Delete</label></td>
		</tr></dfListItem>
	</tbody>
</table>
</dfInputListCustom>
```
