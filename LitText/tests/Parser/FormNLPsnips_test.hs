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

module Parser.FormNLPsnips_test
--    (module Parser.ProduceDocCallNLP
    where

import           Test.Framework
import Uniform.Test.TestHarness
import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import LitTypes.ServerNames
--import           CoreNLP.Defs0
--import CoreNLP.CoreNLPxml (readDocString)
--import Data.List.Split
--import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
--import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP
import Parser.FormNLPsnips

progName = "tests"
-- instance ShowTestHarness [TextZeile]
instance ShowTestHarness [Snip]


test_1_D_DA = test1File progName "resultD1" "resultDA1" formSnips
test_2_D_DA = test1File progName "resultD2" "resultDA2" formSnips
test_3_D_DA = test1File progName "resultD3" "resultDA3" formSnips
test_4_D_DA = test1File progName "resultD4" "resultDA4" formSnips
test_5_D_DA = test1File progName "resultD5" "resultDA5" formSnips
test_6_D_DA = test1File progName "resultD6" "resultDA6" formSnips
test_8_D_DA = test1File progName "resultD8" "resultDA8" formSnips
test_9_D_DA = test1File progName "resultD9" "resultDA9" formSnips
test_10_D_DA = test1File progName "resultD10" "resultDA10" formSnips
test_11_D_DA = test1File progName "resultD11" "resultDA11" formSnips
test_12_D_DA = test1File progName "resultD12" "resultDA12" formSnips



---- test mergeNLP
--text1 = tz3fillLength $ Snip {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
--        tz3text = LCtext "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme ."  German}
--text2 = tz3fillLength $ Snip {tz3loc = TextLoc {tlpage = Just "7", tlline = 59}, tz3para = ParaNum 11,
--        tz3text = LCtext "die ich dann mit Schminke korrigierte."  German}
--t12 =  Just . tz3fillLength $
--  (Snip{tz3loc = TextLoc{tlpage = Just "7", tlline = 59},
--           tz3para = ParaNum 11,
--           tz3text = LCtext
--             "Neben dem Spiegel hing in einem Rahmen eine Portraitaufnahme . die ich dann mit Schminke korrigierte."
--              German})
--
--test_mergeNLP = assertEqual    t12  (mergeNLPtext text1 text2)


