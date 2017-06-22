---------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import Uniform.Strings
import                   Test.Framework
--import {-@ HTF_TESTS @-} Parser.ReadMarkupAB
--import {-@ HTF_TESTS @-} BuchCode.MarkupText
--import   {-@ HTF_TESTS @-} Lines2para.HandleLayout
--import   {-@ HTF_TESTS @-} Lines2para.Lines2ignore
import   {-@ HTF_TESTS @-} Parser.ProduceLayout
--import   {-@ HTF_TESTS @-} Lines2para.Lines2para
--import   {-@ HTF_TESTS @-} Parser.ProduceLit
----
--import   {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP   -- calls to NLP, takes time
--                -- tests only the production of the doc files
--import   {-@ HTF_TESTS @-} Parser.CompleteSentence
-------- calls 17701
--import   {-@ HTF_TESTS @-} Parser.ProduceNLP
--import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples

-- old
----import {-@ HTF_TESTS @-} Processor.CheckServers
----import   {-@ HTF_TESTS @-} Parser.ConvertTaggerOutput
------ no test for NLPvocabulary
--
--
---- makes call to NLP
----import {-@ HTF_TESTS @-} Main2sub
--import {-@ HTF_TESTS @-} Parser.Main2subTest
--import  {-@ HTF_TESTS @-} Processor.ProcessAll

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "Lit Text Test.hs end ------------- \n"
    return ()


