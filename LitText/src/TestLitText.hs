-----------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import Uniform.Strings
import           Test.Framework
import {-@ HTF_TESTS @-} Parser.ReadMarkupAB
import {-@ HTF_TESTS @-} BuchCode.MarkupText
import   {-@ HTF_TESTS @-} Lines2para.HandleLayout
--import   {-@ HTF_TESTS @-} Lines2para.Lines2para
--import   {-@ HTF_TESTS @-} Parser.ProduceLit
--import   {-@ HTF_TESTS @-} Parser.ProduceNLP
------ makes call to NLP
--import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples
----import   {-@ HTF_TESTS @-} CoreNLP.Snippets2nt  -- no tests?
----import {-@ HTF_TESTS @-} Main2sub
--import {-@ HTF_TESTS @-} Parser.Main2subTest
--import  {-@ HTF_TESTS @-} Processor.ProcessAll

main =  do
    putStrLn "Lit Text Test.hs:\n"
    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    return ()


