{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Internal.Splice
	( AppendableSplice
	, runSplices
	, runSplicesWith
	, addSplices
	, mergeSplices
	) where

import Data.Map.Syntax ((##))
import Data.Monoid (mempty)
import Data.Text (Text)
import Text.Digestive.View (View)
import Heist (Splices)
import Heist.Interpreted

----------------------------------------------------------------------

type AppendableSplice m = (View Text -> Splices (Splice m)) -> View Text -> Splices (Splice m)

runSplices :: Monad m => AppendableSplice m -> View Text -> Splices (Splice m)
runSplices splices v = splices mempty v

runSplicesWith :: Monad m => AppendableSplice m -> (View Text -> Splices (Splice m)) -> View Text -> Splices (Splice m)
runSplicesWith splices s v = do
	splices s v
	s v

addSplices :: Monad m => AppendableSplice m -> (View Text -> Splices (Splice m)) -> AppendableSplice m
addSplices splices moreSplices = go
	where
--		go :: Monad m => AppendableSplice m
		go s v = do
			splices (runSplicesWith go s) v
			moreSplices v

mergeSplices :: Monad m => AppendableSplice m -> AppendableSplice m -> AppendableSplice m
mergeSplices splices moreSplices = go
	where
--		go :: Monad m => AppendableSplice m
		go s v = do
			splices (go s) v
			moreSplices (go s) v
