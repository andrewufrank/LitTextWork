-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import           Uniform.Strings
import           Test.Framework
------import {-@ HTF_TESTS @-} CmdLineUtilities.UtilsParseArgs_test
----import {-@ HTF_TESTS @-} Data.RDFext_test
----
------import {-@ HTF_TESTS @-} xx

-- for CoreNLP:
--import {-@ HTF_TESTS @-} LitText.CoreNLP.DocBase_test
--import {-@ HTF_TESTS @-} LitText.CoreNLP.Vocabulary_test
--import {-@ HTF_TESTS @-} LitText.CoreNLP.ParseJsonCoreNLP_test
--import {-@ HTF_TESTS @-} LitText.CoreNLP.DocNLP_0or1_test
--import {-@ HTF_TESTS @-} LitText.CoreNLP.Doc1_absoluteID_test
--import {-@ HTF_TESTS @-} LitText.CoreNLP.Doc2ToLinear_test
import {-@ HTF_TESTS @-} LitText.CoreNLP.Linear2Triple_test

main =  do
    putStrLn "LitFoundation.hs:\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
