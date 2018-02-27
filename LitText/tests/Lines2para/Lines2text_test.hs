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
import Parser.TextDescriptor -- (ParaNum (..), unparaNum)
import Lines2para.Lines2text

----test_0B_C = testFile2File "resultBAC0" "resultBAD0" text2tz1
test_1B_C = testFile2File "resultB1" "resultC1" text2tz1
test_2B_C = testFile2File "resultB2" "resultC2" text2tz1
test_3B_C = testFile2File "resultB3" "resultC3" text2tz1
test_4B_C = testFile2File "resultB4" "resultC4" text2tz1
test_5B_C = testFile2File "resultB5" "resultC5" text2tz1
test_6B_C = testFile2File "resultB6" "resultC6" text2tz1
test_8B_C = testFile2File "resultB8" "resultC8" text2tz1
test_9B_C = testFile2File "resultB9" "resultC9" text2tz1
test_10B_C = testFile2File "resultB10" "resultC10" text2tz1
test_11B_C = testFile2File "resultB11" "resultC11" text2tz1
test_12B_C = testFile2File "resultB12" "resultC12" text2tz1


----test_0BAC_BAD = testFile2File "resultBAC0" "resultBAD0" paragraphs2TZsimple
test_1BAC_BAD = testFile2File "resultBAC1" "resultBAD1" paragraphs2TZsimple
test_2BAC_BAD = testFile2File "resultBAC2" "resultBAD2" paragraphs2TZsimple
test_3BAC_BAD = testFile2File "resultBAC3" "resultBAD3" paragraphs2TZsimple
test_4BAC_BAD = testFile2File "resultBAC4" "resultBAD4" paragraphs2TZsimple
test_5BAC_BAD = testFile2File "resultBAC5" "resultBAD5" paragraphs2TZsimple
test_6BAC_BAD = testFile2File "resultBAC6" "resultBAD6" paragraphs2TZsimple
test_8BAC_BAD = testFile2File "resultBAC8" "resultBAD8" paragraphs2TZsimple
test_9BAC_BAD = testFile2File "resultBAC9" "resultBAD9" paragraphs2TZsimple
test_10BAC_BAD = testFile2File "resultBAC10" "resultBAD10" paragraphs2TZsimple
test_11BAC_BAD = testFile2File "resultBAC11" "resultBAD11" paragraphs2TZsimple
test_12BAC_BAD = testFile2File "resultBAC12" "resultBAD12" paragraphs2TZsimple

------------LANGUAGE



