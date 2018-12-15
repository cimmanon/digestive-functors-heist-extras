{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Internal.Splice
	( AppendableSplices
	, addSplices
	, addSplicesWith
	) where

import Data.Map.Syntax (mapV)
import Data.Monoid (mempty)
import Data.Text (Text)
import Text.Digestive.View (View)
import Heist (Splices)
import Heist.Interpreted

----------------------------------------------------------------------

type AppendableSplices m = Splices (View Text -> Splice m) -> View Text -> Splices (Splice m)

addSplices :: Monad m
	=> Splices ((Splices (View Text -> Splice m) -> View Text -> Splices (Splice m)) -> View Text -> Splice m)
	-> AppendableSplices m
addSplices = addSplicesWith mempty

addSplicesWith :: Monad m
	=> (View Text -> Splices (Splice m))
	-> Splices ((Splices (View Text -> Splice m) -> View Text -> Splices (Splice m)) -> View Text -> Splice m)
	-> AppendableSplices m
addSplicesWith nonExtendableSplices baseSplices moreSplices view = do
	let
	--	newSplices :: Monad m => Splices (View Text -> Splice m) -> View Text -> Splices (Splice m)
		newSplices = do
			baseSplices
			mapV const moreSplices

	nonExtendableSplices view
	mapV (\splice -> splice (addSplicesWith nonExtendableSplices newSplices) view) newSplices
