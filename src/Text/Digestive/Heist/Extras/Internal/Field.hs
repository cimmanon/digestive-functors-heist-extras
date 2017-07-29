{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types                #-}

module Text.Digestive.Heist.Extras.Internal.Field
	( queryField'
	, lookupInput
	) where

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
