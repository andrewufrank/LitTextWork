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

module LitText.Parser.FormNLPsnips_test
--    (module LitText.Parser.ProduceDocCallNLP
    where

import           Test.Framework
import Uniform.Test.TestHarness
import Data.Maybe -- todo
import LitText.Lines
import LitText.Parser

progName = "tests"
-- instance ShowTestHarness [TextZeile]
instance ShowTestHarness [Snip]
instance ShowTestHarness [TZ2]
instance ShowTestHarness TextDescriptor

snip4test :: TextDescriptor -> [TZ2] -> [Snip]  -- change to TZ1 -> Snip
snip4test    = tz2toSnip
----
test_1_C_D = test2File progName "resultA1" "resultCA1" "resultD1" snip4test
test_2_C_D = test2File progName "resultA2" "resultCA2" "resultD2" snip4test
test_3_C_D = test2File progName "resultA3" "resultCA3" "resultD3" snip4test
test_4_C_D = test2File progName "resultA4" "resultCA4" "resultD4" snip4test
test_5_C_D = test2File progName "resultA5" "resultCA5" "resultD5" snip4test
test_6_C_D = test2File progName "resultA6" "resultCA6" "resultD6" snip4test
test_8_C_D = test2File progName "resultA8" "resultCA8" "resultD8" snip4test
test_9_C_D = test2File progName "resultA9" "resultCA9" "resultD9" snip4test
test_10_C_D = test2File progName "resultA10" "resultCA10" "resultD10" snip4test
test_11_C_D = test2File progName "resultA11" "resultCA11" "resultD11" snip4test
test_12_C_D = test2File progName "resultA12" "resultCA12" "resultD12" snip4test

--formSnips :: [Snip] -> [Snip]

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


