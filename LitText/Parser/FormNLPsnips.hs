 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FormNLPsnips  D -> DA
-- Copyright   :  andrew u frank -
--
-- | form pieces of literal text which are reasonably sized

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FormNLPsnips
--    (module Parser.ProduceDocCallNLP
--    , module CoreNLP.Defs0
--    , module Lines2para.Lines2para
--    , module Producer.Servers
--    , module Parser.FilterTextForNLP
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
import Parser.FilterTextForNLP

formSnips :: [Snip] -> [Snip]
-- collect paragraphis in reasonalbe snips for NLP processing
formSnips [] = []
formSnips [n] = [n]
formSnips (n1:n2:ns) =  case mergeNLPtext n1 n2 of
        Nothing -> n1 : formSnips (n2:ns)
        Just ab ->  formSnips (ab:ns)


minSnipSize = 1000 -- 5000 -- char
maxSnipSize = 2000  -- 10000

test_1_D_DA = testFile2File "resultD1" "resultDA1" formSnips
test_2_D_DA = testFile2File "resultD2" "resultDA2" formSnips
test_3_D_DA = testFile2File "resultD3" "resultDA3" formSnips
test_4_D_DA = testFile2File "resultD4" "resultDA4" formSnips
test_5_D_DA = testFile2File "resultD5" "resultDA5" formSnips
test_6_D_DA = testFile2File "resultD6" "resultDA6" formSnips
test_8_D_DA = testFile2File "resultD8" "resultDA8" formSnips
test_9_D_DA = testFile2File "resultD9" "resultDA9" formSnips
test_10_D_DA = testFile2File "resultD10" "resultDA10" formSnips
test_11_D_DA = testFile2File "resultD11" "resultDA11" formSnips
test_12_D_DA = testFile2File "resultD12" "resultDA12" formSnips

mergeNLPtext :: Snip -> Snip -> Maybe Snip
-- merge two text if same language and size less than maxSnipSize
mergeNLPtext a b = if sameLang a b && tz3textLength a + tz3textLength b < maxSnipSize
                        then Just (a{tz3text = tz3text a <> " " <> tz3text b
                                , tz3textLength = tz3textLength a + tz3textLength b +1})
                                    -- there is a spaece added, would a period "." be needed?
                        else Nothing

    where
        alength = lengthChar . tz3text $ a
        blength = lengthChar . tz3text $ b
        sameLang a b = tz3lang a == tz3lang b

-- test mergeNLP
text1 = tz3fillLength $ Snip {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
        tz3text = "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme .", tz3lang = German}
text2 = tz3fillLength $ Snip {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
        tz3text = "die ich dann mit Schminke korrigierte.", tz3lang = German}
t12 =  Just . tz3fillLength $
  (Snip{tz3loc = TextLoc{tlpage = Just "7", tlline = 59},
           tz3para = ParaNum 11,
           tz3text =
             "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme . die ich dann mit Schminke korrigierte.",
           tz3lang = German})

test_mergeNLP = assertEqual    t12  (mergeNLPtext text1 text2)


