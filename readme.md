This is a collection of custom splices for use with Digestive Functors Heist.  Some of the default splices generate invalid markup or don't play nicely with the newer HTML5 form attributes like `required`.

These are intended to be used with either `digestiveSplices'` or `bindDigestiveSplices'`, which can be found in the `custom-splices` branch of [digestive-functors](https://github.com/cimmanon/digestive-functors) for the digestive-functors-heist library.

# Using the splices

There's no bindings, you'll have to create your own before you can use them.

```haskell
digestiveSplicesCustom :: MonadIO m => View Text -> Splices (Splice m)
digestiveSplicesCustom v = do
    digestiveSplices v
    "dfPlainText" ## dfPlainText v
    "dfInputListCustom" ## dfInputListCustom digestiveSplicesCustom v

fooH :: AppHandler ()
fooH = do
    (view, result) <- runForm "form" fooForm
    renderWithSplices "path/to/template" $ digestiveSplicesCustom view
```
