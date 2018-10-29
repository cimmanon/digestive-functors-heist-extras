{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Debug
	( dfShowView
	) where

import Data.Map.Syntax ((##))
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted
import Text.Digestive.View (View)
import qualified Text.XmlHtml as X

----------------------------------------------------------------------
-- Shows the current View

dfShowView :: Monad m => View Text -> Splice m
dfShowView view = return [X.Element "pre" [] [X.TextNode $ T.pack $ show view]]
