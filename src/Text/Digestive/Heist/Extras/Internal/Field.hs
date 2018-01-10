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
	, fieldInputChoice
	, fieldInputChoiceMultiple
	, fieldInputChoiceGroup
	, fieldInputChoiceGroupMultiple
	) where

import Control.Arrow (second)
import Control.Monad.Identity (Identity (..))
import Data.Text (Text)
import Data.Monoid ((<>))
import Text.Digestive.Form
import Text.Digestive.Types (Path, fromPath, FormInput)
import Text.Digestive.Form.Internal
import Text.Digestive.Form.Internal.Field

--------------------------------------------------------------------------------
import qualified Data.Text as T
import Text.Digestive.Types (toPath)
import Text.Digestive.View hiding (fieldInputChoice, fieldInputChoiceGroup)

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

{----------------------------------------------------------------------------------------------------{
                                                                      | Alternate functions querying choice fields
}----------------------------------------------------------------------------------------------------}

-- Note that these are modified versions of the ones found in Text.Digestive.View,
-- they fix some issues related to the addition of choiceMultiple.  Yes, I know
-- there is duplicate code here, but it's just a test before pushing upstream

--------------------------------------------------------------------------------
-- | Returns a list of (identifier, view, selected?)
fieldInputChoice :: forall v. Text -> View v -> [(Text, v, Bool)]
fieldInputChoice = fieldInputChoiceI False

fieldInputChoiceMultiple :: forall v. Text -> View v -> [(Text, v, Bool)]
fieldInputChoiceMultiple = fieldInputChoiceI True

fieldInputChoiceI :: forall v. Bool -> Text -> View v -> [(Text, v, Bool)]
fieldInputChoiceI multiple ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> [(Text, v, Bool)]
    eval' field = case field of
        Choice xs didx ->
            let
              idx = map snd (evalField method givenInput (Choice xs didx))
              choices = if not multiple && null idx then didx else idx
             in map (\(i, (k, (_, v))) -> (k, v, i `elem` choices)) $
                 zip [0 ..] $ concat $ map snd xs
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"

--------------------------------------------------------------------------------
-- | Returns a list of (groupName, [(identifier, view, selected?)])
fieldInputChoiceGroup :: forall v. Text -> View v -> [(Text, [(Text, v, Bool)])]
fieldInputChoiceGroup = fieldInputChoiceGroupI False

fieldInputChoiceGroupMultiple :: forall v. Text -> View v -> [(Text, [(Text, v, Bool)])]
fieldInputChoiceGroupMultiple = fieldInputChoiceGroupI True

fieldInputChoiceGroupI :: forall v. Bool
                      -> Text
                      -> View v
                      -> [(Text, [(Text, v, Bool)])]
fieldInputChoiceGroupI multiple ref (View _ _ form input _ method) =
    queryField path form eval'
  where
    path       = toPath ref
    givenInput = lookupInput path input

    eval' :: Field v b -> [(Text, [(Text, v, Bool)])]
    eval' field = case field of
        Choice xs didx ->
            let
              idx = map snd (evalField method givenInput (Choice xs didx))
              choices = if not multiple && null idx then didx else idx
            in merge choices xs [0..]
        f           -> error $ T.unpack ref ++ ": expected (Choice _ _), " ++
            "but got: (" ++ show f ++ ")"

merge :: [Int]
      -> [(Text, [(Text, (a, v))])]
      -> [Int]
      -> [(Text, [(Text, v, Bool)])]
merge _ [] _ = []
merge idx (g:gs) is = cur : merge idx gs b
  where
    (a,b) = splitAt (length $ snd g) is
    cur = (fst g, map (\(i, (k, (_, v))) -> (k, v, i `elem` idx)) $ zip a (snd g))
