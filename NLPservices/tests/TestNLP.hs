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

--import {-@ HTF_TESTS @-} CoreNLP.CoreNLPxml_test
--import {-@ HTF_TESTS @-} CoreNLP.Defs0_test
--import {-@ HTF_TESTS @-} CoreNLP.Models_test
-- parser:
import {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test
-- tests text to snip (M) and to Triples (N)

--import {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test
--import {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test
--import {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test

--import {-@ HTF_TESTS @-} CoreNLP.ParseJsonCoreNLP_test
--import {-@ HTF_TESTS @-} CoreNLP.Doc2ToDoc0_test
----------import {-@ HTF_TESTS @-} CoreNLP.ProduceNLPtriples2_testX
            -- uses all memory - must be run in main!

main =  do
    putStrLn "TestNLP.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "TestNLP.hs end ------------- \n"
    return ()



