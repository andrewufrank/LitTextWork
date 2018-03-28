 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FilterTextForNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed
-- each paragraph is in a single snip
-- snips are merged later

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    , RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FilterTextForNLP (
     prepareTZ4nlp
    , TZ2, Snip
    ) where

import Data.Maybe (catMaybes) -- todo
import BuchCode.Classes4text
import LitTypes.TextDescriptor

-------------- prepare the text - which is conversion to NLPtext  -- BAE -> D


---- | a single language piece of text with lanuage code,
--             length and start para number
--data Snip = Snip { snip3loc :: TextLoc
----                        , snip3para :: ParaNum
--                        , snip3snipnr ::  SnipID
--                        , snip3baserdf :: PartURI
----                        , snip3snipsigl :: SnipSigl
----                        , snip3parasigl :: ParaSigl
--                        , snip3text:: LCtext
--                        , snip3textLength :: Int
--                        , snip3posTag :: Text
----                        , snip3lang :: LanguageCode
--                        }
--            deriving (Read, Show, Eq )

tz3fillLength :: Snip -> Snip
-- fill the length field
tz3fillLength n = n{snip3textLength = getLengthLC . snip3text $ n}


prepareTZ4nlp :: Text -> [TZ2] -> [Snip]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlp postag tz2s = map tz3fillLength . catMaybes
        . map (prepareTZ4nlpOne postag) $ tz2s


prepareTZ4nlpOne :: Text -> TZ2 -> Maybe Snip  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlpOne postag tz2 = if condNLPtext tz2 then Just $ formatParaText postag tz2
                    else Nothing

--prepareTZ4nlp = map formatParaText . filter condNLPtext
        ---------------------------------preparing for analysis

condNLPtext :: TZ2 -> Bool
-- select the paragraphs and the titles to TZtext
condNLPtext tz  = case tz of
--    TZzdahl {}  -> errorT ["condNLPtext","should not have TZzahl left", showT tz]
    TZ2markup {} ->
            case tz2tok tz of
                BuchTitel ->  True
                BuchHL1   ->  True
                BuchHL2   ->  True
                BuchHL3   ->  True
                _         ->   False
    TZ2para {} -> True

formatParaText :: Text -> TZ2 -> Snip
-- convert the headers to a tztext
formatParaText postag tz@TZ2para{..} =
        Snip {
            snip3loc = tz2loc
--                , tz3para = tz2para
--                , snip3snipnr = zero  -- not acceptable snip nr, cannot be undef
                                -- would not work with test harness
--                , tz3snipsigl = zero
            , snip3posTag = postag
            , snip3text = codeText lang (foldl1 combine2linesWithHyphenation
                        . map (getText . twm1 . tztext1) $ (tz2tzs  ))
        }
    where
            lang  = getLanguageCode . twm1 . tztext1
                        . headNote "formatParaText lang" $ tz2tzs   :: LanguageCode

formatParaText postag tz@TZ2markup {..} =
    Snip {snip3loc = tz2loc
--        , tz3lang = tz2lang
--        , tz3para = tz2para
        , snip3snipnr = zero
--        , tz3snipsigl = zero
        , snip3text = codeText lang $ tx <> ". "     -- to make sure these are sentences for NLP
                                      --    risk of two ..
        , snip3posTag = postag
        }

    where
        tx = getText . twm1 $ tz2text
        lang = getLanguageCode . twm1 $ tz2text


