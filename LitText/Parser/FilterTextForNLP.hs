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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FilterTextForNLP
--    (module Parser.ProduceDocCallNLP
--    , module CoreNLP.Defs0
--    , module Lines2para.Lines2para
--    , module Producer.Servers
--    )
    where

--import           Test.Framework
--import Uniform.TestHarness
import Data.Maybe -- todo
import Lines2para.Lines2para
--import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import Producer.Servers
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
import Data.List.Split
import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
import Text.Regex (mkRegex, subRegex)
import BuchCode.BuchToken
import Lines2para.MarkupText (combine2linesWithHyphenation)

-------------- prepare the text - which is conversion to NLPtext  -- BAE -> D


---- | a single language piece of text with lanuage code, length and start para number
--data Snip = Snip { tz3loc :: TextLoc
--                        , tz3para :: ParaNum
--                        , tz3text:: Text
--                        , tz3textLength :: Int
--                        , tz3lang :: LanguageCode }
--            deriving (Read, Show, Eq )

tz3fillLength :: Snip -> Snip
-- fill the length field
tz3fillLength n = n{tz3textLength = getLengthLC . tz3text $ n}


prepareTZ4nlp :: Text -> [TZ2] -> [Snip]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlp postag tz2s = map tz3fillLength . catMaybes . map (prepareTZ4nlpOne postag) $ tz2s


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
formatParaText postag tz@TZ2para{} = Snip {
                tz3loc = tz2loc tz
                , tz3para = tz2para tz
        , tz3posTag = postag
                , tz3text = codeText lang (foldl1 combine2linesWithHyphenation
            . map (getText . twm1 . tztext1) $ (tz2tzs tz))
        }
    where
            lang  = getLanguageCode . twm1 . tztext1
                        . headNote "formatParaText lang" . tz2tzs $ tz  :: LanguageCode

formatParaText postag tz@TZ2markup {} = Snip {tz3loc = tz2loc tz
--        , tz3lang = tz2lang tz
        , tz3para = tz2para tz
        , tz3text = codeText lang $ tx <> ". "     -- to make sure these are sentences for NLP
                                      --    risk of two ..
        , tz3posTag = postag
        }

    where
        tx = getText . twm1 . tz2text $ tz
        lang = getLanguageCode . twm1 . tz2text $ tz


