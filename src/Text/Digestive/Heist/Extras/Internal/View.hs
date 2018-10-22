{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Text.Digestive.Heist.Extras.Internal.View
	( disableView
	) where

import Data.List ((\\))
import Text.Digestive.Form.Internal (FormTree(..), Metadata(..))
import Text.Digestive.View

----------------------------------------------------------------------

disableView :: View a -> View a
disableView (View n c f i e m) = View n c (disableRecursive f) i e m

disableRecursive :: Functor t => FormTree t v m a -> FormTree t v m a
disableRecursive (Ref r x) = Metadata [Disabled] $ Ref r $ disableRecursive x
disableRecursive x@(Pure _) = x
disableRecursive (App x y) = App (disableRecursive x) (disableRecursive y)
disableRecursive (Map g x) = Map g $ disableRecursive x
disableRecursive (Monadic x) = Monadic $ fmap disableRecursive x
disableRecursive (List d xs) = List (fmap disableRecursive d) xs
disableRecursive (Metadata m1 (Metadata m2 x)) = Metadata (m1 \\ m2) (disableRecursive x)
-- ^ if we find a nested Metadata, merge them together
disableRecursive (Metadata m (Ref r x)) = Metadata ([Disabled] \\ m) $ Ref r (disableRecursive x)
-- ^ if the Metadata contains a Ref, make sure it is disabled before we continue down the tree
disableRecursive (Metadata m x) = Metadata m (disableRecursive x)
