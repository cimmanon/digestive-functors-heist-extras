{-# LANGUAGE OverloadedStrings #-}

module Text.Digestive.Heist.Extras.Debug
	( dfShowView
	, dfPathList
	) where

import Data.Map.Syntax ((##))
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted
import Text.Digestive.Types (fromPath)
import Text.Digestive.View (View, debugViewPaths)
import qualified Text.XmlHtml as X

----------------------------------------------------------------------
-- Shows the current View

dfShowView :: Monad m => View Text -> Splice m
dfShowView view = return [X.Element "pre" [] [X.TextNode $ T.pack $ show view]]

----------------------------------------------------------------------
-- Provides a list of possible paths that are visible from the current view

dfPathList :: Monad m => View Text -> Splice m
dfPathList view = mapSplices (\x -> runChildrenWith $ "path" ## textSplice $ fromPath x) $ debugViewPaths view
