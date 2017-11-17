{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Svg ( elSvgns, svgEl ) where

import Data.Map (Map)
import Data.Text (Text)

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

svgEl :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgEl t attrs content = fmap snd $ elSvgns t attrs content
