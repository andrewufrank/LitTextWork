-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  test NLP services
-- Copyright   :  andrew u frank -
--


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import  Uniform.Strings
import   Test.Framework

--import {-@ HTF_TESTS @-} CoreNLP.Doc2ToLinear_test
--import {-@ HTF_TESTS @-} CoreNLP.DocBase_test
--import {-@ HTF_TESTS @-} CoreNLP.Vocabulary_test
import {-@ HTF_TESTS @-} CoreNLP.ParseJsonCoreNLP_test

--import {-@ HTF_TESTS @-} CoreNLP.DocNLP_0or1_test
--import {-@ HTF_TESTS @-} CoreNLP.Doc1_absoluteID_test
--import {-@ HTF_TESTS @-} CoreNLP.Doc2ToLinear_test
--import {-@ HTF_TESTS @-} CoreNLP.Linear2Triple_test
-- parser:
--import {-@ HTF_TESTS @-} NLP2RDF.ProduceDocCallNLP_test
-- tests text to snip (M) and to Triples (N)

--import {-@ HTF_TESTS @-} NLP2RDF.ProduceDocCallNLP_test
--import {-@ HTF_TESTS @-} NLP2RDF.ProduceDocCallNLP_test
--import {-@ HTF_TESTS @-} NLP2RDF.ProduceDocCallNLP_test

--import {-@ HTF_TESTS @-} CoreNLP.ParseJsonCoreNLP_test
--import {-@ HTF_TESTS @-} CoreNLP.Doc2ToDoc1_test
----------import {-@ HTF_TESTS @-} CoreNLP.ProduceNLPtriples2_testX
            -- uses all memory - must be run in main!

main =  do
    putStrLn "TestNLP.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "TestNLP.hs end ------------- \n"
    return ()



