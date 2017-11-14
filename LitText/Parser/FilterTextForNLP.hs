 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FilterTextForNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

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

import           Test.Framework
import Uniform.TestHarness
import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import Producer.Servers
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
import Data.List.Split
import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
import Text.Regex (mkRegex, subRegex)



-------------- prepare the text - which is conversion to NLPtext  -- BAE -> D


data NLPtext = NLPtext { tz3loc :: TextLoc
                        , tz3para :: ParaNum
                        , tz3text:: Text
                        , tz3lang :: LanguageCode }
            deriving (Read, Show, Eq )

instance Zeros NLPtext where zero = NLPtext zero zero zero zero
--instance (Zeros a) => Zeros (Maybe a) where zero = Nothing
-- todo algebra

prepareTZ4nlp :: [TZ2] -> [NLPtext]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlp tz2s = catMaybes . map prepareTZ4nlpOne $ tz2s


prepareTZ4nlpOne :: TZ2 -> Maybe NLPtext  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlpOne tz2 = if condNLPtext tz2 then Just $ formatParaText tz2
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

formatParaText :: TZ2 -> NLPtext
-- convert the headers to a tztext
formatParaText tz@TZ2para{} = NLPtext {
                tz3loc = tz2loc tz
                , tz3para = tz2para tz
                , tz3lang = tz2lang tz
                , tz3text = foldl1 combine2linesWithHyphenation
            . map (twm . tztext) $ (tz2tzs tz)
        }

formatParaText tz@TZ2markup {} = NLPtext {tz3loc = tz2loc tz
        , tz3lang = tz2lang tz
        , tz3para = tz2para tz
        , tz3text =  twm . tz2text $ tz}

test_1_C_D = testFile2File "resultBAE1" "resultD1" ( prepareTZ4nlp)
test_2_C_D = testFile2File "resultBAE2" "resultD2" ( prepareTZ4nlp)
test_3_C_D = testFile2File "resultBAE3" "resultD3" ( prepareTZ4nlp)
test_4_C_D = testFile2File "resultBAE4" "resultD4" ( prepareTZ4nlp)
test_5_C_D = testFile2File "resultBAE5" "resultD5" ( prepareTZ4nlp)
test_6_C_D = testFile2File "resultBAE6" "resultD6" ( prepareTZ4nlp)
test_8_C_D = testFile2File "resultBAE8" "resultD8" ( prepareTZ4nlp)
test_9_C_D = testFile2File "resultBAE9" "resultD9" ( prepareTZ4nlp)
test_10_C_D = testFile2File "resultBAE10" "resultD10" ( prepareTZ4nlp)


