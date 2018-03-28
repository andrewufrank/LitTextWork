-----------------------------------------------------------------------------
--
-- Module      :  Parser . deal with ignored lines
-- Copyright   :  andrew u frank -
--
-- |  deal wit ignored lines and language
-- ignore must ignore lines, even if they have a markup
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2text_test where


--import BuchCode.MarkupText
--import BuchCode.BuchToken
import Lines2para.HandleLayout

--import           Data.List.Split
-- TODO string s
--import Data.List (nub)
import           Test.Framework
import Uniform.TestHarness
import LitTypes.TextDescriptor -- (ParaNum (..), unparaNum)
import Lines2para.Lines2text

--text2tz1 :: Text -> [TZ1]  -- test B -> C

----test_0B_C = test1File "resultBAC0" "resultBAD0" text2tz1
test_1B_C = test1File "resultB1" "resultC1" text2tz1
test_2B_C = test1File "resultB2" "resultC2" text2tz1
test_3B_C = test1File "resultB3" "resultC3" text2tz1
test_4B_C = test1File "resultB4" "resultC4" text2tz1
test_5B_C = test1File "resultB5" "resultC5" text2tz1
test_6B_C = test1File "resultB6" "resultC6" text2tz1
test_8B_C = test1File "resultB8" "resultC8" text2tz1
test_9B_C = test1File "resultB9" "resultC9" text2tz1
test_10B_C = test1File "resultB10" "resultC10" text2tz1
test_11B_C = test1File "resultB11" "resultC11" text2tz1
test_12B_C = test1File "resultB12" "resultC12" text2tz1

--paragraphs2TZsimple :: [TZ] -> [TZ1]  -- test BA -> BC

----test_0BB_BC = test1File "resultBB0" "resultBC0" paragraphs2TZsimple
test_1BB_BC = test1File "resultBB1" "resultBC1" paragraphs2TZsimple
test_2BB_BC = test1File "resultBB2" "resultBC2" paragraphs2TZsimple
test_3BB_BC = test1File "resultBB3" "resultBC3" paragraphs2TZsimple
test_4BB_BC = test1File "resultBB4" "resultBC4" paragraphs2TZsimple
test_5BB_BC = test1File "resultBB5" "resultBC5" paragraphs2TZsimple
test_6BB_BC = test1File "resultBB6" "resultBC6" paragraphs2TZsimple
test_8BB_BC = test1File "resultBB8" "resultBC8" paragraphs2TZsimple
test_9BB_BC = test1File "resultBB9" "resultBC9" paragraphs2TZsimple
test_10BB_BC = test1File "resultBB10" "resultBC10" paragraphs2TZsimple
test_11BB_BC = test1File "resultBB11" "resultBC11" paragraphs2TZsimple
test_12BB_BC = test1File "resultBB12" "resultBC12" paragraphs2TZsimple

------------LANGUAGE



