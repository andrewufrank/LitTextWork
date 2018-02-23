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

module Lines2para.Lines2ignore_test where


--import BuchCode.MarkupText
--import BuchCode.BuchToken
import Lines2para.HandleLayout

--import           Data.List.Split
-- TODO string s
--import Data.List (nub)
import           Test.Framework
import Uniform.TestHarness
import Parser.TextDescriptor -- (ParaNum (..), unparaNum)
import Lines2para.Lines2ignore



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



-- #include "Lines2ignoreTestResults.res"

test_german1 = assertEqual German (readLanguageCode "test_german1" "Deutsch")
test_german2 = assertEqual German (readLanguageCode "test_german1" "German")
