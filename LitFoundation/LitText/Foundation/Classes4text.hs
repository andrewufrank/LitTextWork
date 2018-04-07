-----------------------------------------------------------------------------
--
-- Module      :  Main.BuchToken

--
-- | the different types of text in a markup file
-- TODO would it be ossible to select lines with all caps as hl1?
-- recognize the ending of gutenberg to ignore?
-- uebersetzung und uebersetzer?
    -- for dublin core see <https://de.wikipedia.org/wiki/Dublin_Core>
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module LitText.Foundation.Classes4text (module LitText.Foundation.Classes4text
        , module LitText.Foundation.BuchToken
            ) where

import LitText.Foundation.BuchToken hiding (try)



class Zeilen z where
-- ^ ops on zeilen
    isLeerzeile
        , isSeitenzahl
        , isTextZeile   -- ^ any line containing lit text
        , isTextZeileIncludeInPara -- ^ only the text lines which can aggregate in paragraph
        , isNeueSeite
        , isMarkupZeile
        , isKurzeZeile  :: z -> Bool
    isMarkupX :: BuchToken -> z -> Bool
    zeilenText :: z -> Text
    -- returns just the text field

    renderZeile :: z -> Text -- ^ gives the literary text

combine2linesWithHyphenation :: Text -> Text -> Text
-- combine two words or parts of words
combine2linesWithHyphenation a b =
        maybe (unwordsT [a,b])  (<>b)  $ stripSuffix' "-" a
