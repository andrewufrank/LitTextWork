 -----------------------------------------------------------------------------
--
-- Module      :  Parser . FilterTextForNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed
-- each paragraph is in a single snip
-- snips are merged later

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.FilterTextForNLP_test
    where

import           Test.Framework
import Uniform.TestHarness

import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import LitTypes.ServerNames
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
--import Data.List.Split
--import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
--import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP

prepareTZ4nlpTest :: [TZ2] -> [Snip]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlpTest  = prepareTZ4nlp ""
--    map tz3fillLength . catMaybes . map (prepareTZ4nlp "")




test_1_CA_DA = testFile2File "resultCA1" "resultD1" prepareTZ4nlpTest
test_2_C_D = testFile2File "resultCA2" "resultD2" prepareTZ4nlpTest
test_3_C_D = testFile2File "resultCA3" "resultD3" prepareTZ4nlpTest
test_4_C_D = testFile2File "resultCA4" "resultD4" prepareTZ4nlpTest
test_5_C_D = testFile2File "resultCA5" "resultD5" prepareTZ4nlpTest
test_6_C_D = testFile2File "resultCA6" "resultD6" prepareTZ4nlpTest
test_8_C_D = testFile2File "resultCA8" "resultD8" prepareTZ4nlpTest
test_9_C_D = testFile2File "resultCA9" "resultD9" prepareTZ4nlpTest
test_10_C_D = testFile2File "resultCA10" "resultD10" prepareTZ4nlpTest
test_11_C_D = testFile2File "resultCA11" "resultD11" prepareTZ4nlpTest
test_12_C_D = testFile2File "resultCA12" "resultD12" prepareTZ4nlpTest

--snip4test :: [TZ1] -> [Snip]  -- change to TZ1 -> Snip
--snip4test = prepareTZ4nlp "" . paragraphsTZ2TZ2
----
--test_1_C_D = testFile2File "resultC1" "resultD1" snip4test
----test_2_C_D = testFile2File "resultBAE2" "resultD2" snip4test
----test_3_C_D = testFile2File "resultBAE3" "resultD3" snip4test
----test_4_C_D = testFile2File "resultBAE4" "resultD4" snip4test
----test_5_C_D = testFile2File "resultBAE5" "resultD5" snip4test
----test_6_C_D = testFile2File "resultBAE6" "resultD6" snip4test
----test_8_C_D = testFile2File "resultBAE8" "resultD8" snip4test
----test_9_C_D = testFile2File "resultBAE9" "resultD9" snip4test
----test_10_C_D = testFile2File "resultBAE10" "resultD10" snip4test
----test_11_C_D = testFile2File "resultBAE11" "resultD11" snip4test
----test_12_C_D = testFile2File "resultBAE12" "resultD12" snip4test

