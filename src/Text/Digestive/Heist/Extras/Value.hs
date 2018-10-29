{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types                #-}

module Text.Digestive.Heist.Extras.Value
	( dfPathList
	, dfValueList
	) where

import Data.Map.Syntax ((##))
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Heist
import Heist.Interpreted
import Text.Digestive.Types (Method, FormInput, Path, fromPath)
import Text.Digestive.View
import Text.Digestive.Form.Internal.Field

import qualified Text.Digestive.Heist.Extras.Debug as D (dfPathList)
import Text.Digestive.Heist.Extras.Internal.Field

----------------------------------------------------------------------
-- Provides a list of possible paths that are visible from the current view

{-# DEPRECATED dfPathList "The dfPathList has been moved to Text.Digestive.Heist.Extras.Debug" #-}
dfPathList :: Monad m => View T.Text -> Splice m
dfPathList = D.dfPathList

----------------------------------------------------------------------
-- The purpose of this splice is to bind splices with static tag names so that
-- the value of one field can be used as an attribute in another.

dfValueList :: Monad m => View T.Text -> Splices (Splice m)
dfValueList view@(View _ _ form input _ method) =
	let
		paths = debugViewPaths view

		splice :: Monad m => Path -> Splice m
		splice path =
			let
				givenInput = lookupInput path input
			in
				case queryField' path form (fieldSplice method givenInput) of
					Right s -> s
					Left e -> textSplice e
	in
		foldl (\xs x -> do xs; "dfValue:" <> fromPath x ## splice x) mempty paths

fieldSplice :: Monad m => Method -> [FormInput] -> Field v a -> Splice m
fieldSplice method givenInput field = case field of
	Text t -> textSplice $ evalField method givenInput (Text t)
	-- TODO: add splices for remaining Field types, as appropriate
	_      -> return []
