{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types                #-}

module Text.Digestive.Heist.Extras.Internal.Field
	( queryField'
	, lookupInput
	, inChoice
	, inChoiceGroup
	, selectedChoiceValues
	, selectedChoiceGroupValues
	) where

import Control.Arrow (second)
import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Data.Monoid ((<>))
import Text.Digestive.Form
import Text.Digestive.Types (Path, fromPath, FormInput)
import Text.Digestive.Form.Internal
import Text.Digestive.Form.Internal.Field

----------------------------------------------------------------------

-- a variation on the queryField function found in Text.Digestive.Form.Internal
-- returns an Either rather than throwing an exception
queryField' :: Path
		    -> FormTree Identity v m a
		    -> (forall b. Field v b -> c)
		    -> Either Text c
queryField' path form f = case lookupForm path form of
	[]                   -> Left $ ref <> " does not exist"
	(SomeForm form' : _) -> case toField form' of
		Just (SomeField field) -> Right $ f field
		_                      -> Left $ ref <> " is not a field"
	where
		ref = fromPath path

-- function copied from Text.Digestive.View
lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)

----------------------------------------------------------------------

equalChecked :: Eq a => a -> (a, b, Bool) -> Bool
equalChecked x (y, _, checked) = x == y && checked

inChoice :: Eq a => a -> [(a, b, Bool)] -> Bool
inChoice x = any (equalChecked x)

inChoiceGroup :: Eq a => a -> [(c, [(a, b, Bool)])] -> Bool
inChoiceGroup x = any ((inChoice x) . snd)

--

isChecked :: (a, b, Bool) -> Bool
isChecked (_, _, x) = x

selectedChoiceValues :: [(a, b, Bool)] -> [(a, b, Bool)]
selectedChoiceValues = filter isChecked

selectedChoiceGroupValues :: [(a, [(b, c, Bool)])] -> [(a, [(b, c, Bool)])]
selectedChoiceGroupValues = filter (not . null . snd) . map (second (filter isChecked))
