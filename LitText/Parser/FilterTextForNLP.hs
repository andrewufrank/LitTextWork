 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FilterTextForNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed
-- each paragraph is in a single snip
-- snips are merged later

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    , RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Parser.FilterTextForNLP (
     prepareTZ4nlp
    , TZ2, Snip
    ) where

import Data.Maybe (catMaybes) -- todo
import BuchCode.Classes4text
import LitTypes.TextDescriptor

-------------- prepare the text - which is conversion to NLPtext  -- BAE -> D


prepareTZ4nlp :: Text -> RDFsubj -> [TZ2] -> [Snip]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlp postag baserdf tz2s = --map tz3fillLength .
    catMaybes
        . map (prepareTZ4nlpOne postag baserdf) $ tz2s


prepareTZ4nlpOne :: Text -> RDFsubj -> TZ2 -> Maybe Snip  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlpOne postag baserdf tz2 = if condNLPtext tz2
                    then Just $ formatParaText postag baserdf tz2
                    else Nothing

--prepareTZ4nlp = map formatParaText . filter condNLPtext
        ---------------------------------preparing for analysis

condNLPtext :: TZ2 -> Bool
-- select the paragraphs and the titles to TZtext
condNLPtext tz  = case tz of
--    TZzahl {}  -> errorT ["condNLPtext","should not have TZzahl left", showT tz]
    TZ2markup {} ->
            case tz2tok tz of
                BuchTitel ->  True
                BuchHL1   ->  True
                BuchHL2   ->  True
                BuchHL3   ->  True
                _         ->   False
    TZ2para {} -> True

formatParaText :: Text -> RDFsubj -> TZ2 -> Snip
-- convert the headers to a tztext
formatParaText postag baserdf tz@TZ2para{..} =
        Snip {
            snip3loc = tz2loc
--                , tz3para = tz2para
                , snip3snipnr = zero  -- not acceptable snip nr, cannot be undef
                                -- would not work with test harness
            , snip3baserdf = baserdf
--                , tz3snipsigl = zero
            , snip3text = text
            , snip3textLength = getLengthLC text
                    -- is not filled later
            , snip3posTagSetID = postag
        }
    where
            lang  = getLanguageCode . twm1 . tztext1
                        . headNote "formatParaText lang" $ tz2tzs   :: LanguageCode
            text = codeText lang (foldl1 combine2linesWithHyphenation
                        . map (getText . twm1 . tztext1) $ (tz2tzs  ))

formatParaText postag baserdf tz@TZ2markup {..} =
    Snip {snip3loc = tz2loc
--        , tz3lang = tz2lang
--        , tz3para = tz2para
        , snip3snipnr = zero
        , snip3baserdf = baserdf
--        , tz3snipsigl = zero
        , snip3text = text
        , snip3textLength = getLengthLC text
        , snip3posTagSetID = postag
        }

    where
        tx = getText . twm1 $ tz2text
        text = codeText lang $ tx <> ". "
                        -- to make sure these are sentences for NLP
                      --    risk of two ..
        lang = getLanguageCode . twm1 $ tz2text


