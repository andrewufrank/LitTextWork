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

module LitText.Parser.FilterTextForNLP_test
    where

import           Test.Framework
import Uniform.Test.TestHarness

import Data.Maybe -- todo
import LitText.Lines
import LitText.Parser   -- todo  -- for test
--import Text.Regex (mkRegex, subRegex)

progName = "tests"
instance ShowTestHarness [TZ2]
instance ShowTestHarness [Snip]

prepareTZ4nlpTest :: [TZ2] -> [Snip]
-- convert all TZ2 for a text, selecting only literal text
prepareTZ4nlpTest  = prepareTZ4nlp "" (mkRDFsubj (mkIRI "testFilterForNLP"))
-- first "" is postag default - second must be different for each
--    map tz3fillLength . catMaybes . map (prepareTZ4nlp "")




test_1_CA_DA = test1File progName "resultCA1" "resultDZ1" prepareTZ4nlpTest
test_2_C_D = test1File progName "resultCA2" "resultDZ2" prepareTZ4nlpTest
test_3_C_D = test1File progName "resultCA3" "resultDZ3" prepareTZ4nlpTest
test_4_C_D = test1File progName "resultCA4" "resultDZ4" prepareTZ4nlpTest
test_5_C_D = test1File progName "resultCA5" "resultDZ5" prepareTZ4nlpTest
test_6_C_D = test1File progName "resultCA6" "resultDZ6" prepareTZ4nlpTest
test_8_C_D = test1File progName "resultCA8" "resultDZ8" prepareTZ4nlpTest
test_9_C_D = test1File progName "resultCA9" "resultDZ9" prepareTZ4nlpTest
test_10_C_D = test1File progName "resultCA10" "resultDZ10" prepareTZ4nlpTest
test_11_C_D = test1File progName "resultCA11" "resultDZ11" prepareTZ4nlpTest
test_12_C_D = test1File progName "resultCA12" "resultDZ12" prepareTZ4nlpTest

--snip4test :: [TZ1] -> [Snip]  -- change to TZ1 -> Snip
--snip4test = prepareTZ4nlp "" . paragraphsTZ2TZ2
----
--test_1_C_D = test1File progName "resultC1" "resultD1" snip4test
----test_2_C_D = test1File progName "resultBAE2" "resultD2" snip4test
----test_3_C_D = test1File progName "resultBAE3" "resultD3" snip4test
----test_4_C_D = test1File progName "resultBAE4" "resultD4" snip4test
----test_5_C_D = test1File progName "resultBAE5" "resultD5" snip4test
----test_6_C_D = test1File progName "resultBAE6" "resultD6" snip4test
----test_8_C_D = test1File progName "resultBAE8" "resultD8" snip4test
----test_9_C_D = test1File progName "resultBAE9" "resultD9" snip4test
----test_10_C_D = test1File progName "resultBAE10" "resultD10" snip4test
----test_11_C_D = test1File progName "resultBAE11" "resultD11" snip4test
----test_12_C_D = test1File progName "resultBAE12" "resultD12" snip4test

