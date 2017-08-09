{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Internal.Attribute
	( disabledAttr
	, checkedAttr

	, disabledAttrSplice
	, checkedAttrSplice

	, mergeAttrs
	, appendAttr
	, extractAttr
	, getRefAttributes
	) where

import Control.Monad (mplus)
import Data.Function (on)
import Data.List (partition, unionBy)
import Data.Maybe  (fromMaybe)
import Data.Text (Text)
import Heist (AttrSplice, getParamNode, HeistT)
import qualified Text.XmlHtml as X

----------------------------------------------------------------------
-- Commonly used HTML form attributes

disabledAttr :: (Text, Text)
disabledAttr = ("disabled", "")

checkedAttr :: (Text, Text)
checkedAttr = ("checked", "")

----------------------------------------------------------------------
-- Attribute splices

disabledAttrSplice :: Monad m => Bool -> AttrSplice m
disabledAttrSplice x = const $ return $ appendAttr x disabledAttr []

checkedAttrSplice :: Monad m => Bool -> AttrSplice m
checkedAttrSplice x = const $ return $ appendAttr x checkedAttr []

----------------------------------------------------------------------

-- optionally append a value to the beginning of a list
appendAttr :: Bool -> a -> [a] -> [a]
appendAttr True x = (x:)
appendAttr False _ = id

-- turn two collections of attributes into a single collection, where the 2nd
-- list fills in missing values that don't exist in the first
mergeAttrs :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
mergeAttrs = unionBy (on (==) fst)

-- extract a given value out of a list along with the remainder
extractAttr :: Eq a => a -> [(a, b)] -> (b, [(a, b)])
extractAttr k xs =
	let
		(ks, ys) = partition ((== k) . fst) xs
	in
		case ks of
			((_, v):_) -> (v, ys)
			_ -> error "The specified attribute does not exist"
			-- ^ TODO: show the key in the error?

-- copied from Text.Digestive.Heist
getRefAttributes :: Monad m
                 => Maybe Text                       -- ^ Optional default ref
                 -> HeistT m m (Text, [(Text, Text)])  -- ^ (Ref, other attrs)
getRefAttributes defaultRef = do
	node <- getParamNode
	return $ case node of
		X.Element _ as _ ->
			let ref = fromMaybe (error $ show node ++ ": missing ref") $
						lookup "ref" as `mplus` defaultRef
			in (ref, filter ((/= "ref") . fst) as)
		_                -> (error "Wrong type of node!", [])
