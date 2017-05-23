-----------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

import Uniform.Strings
import           Test.Framework
--import {-@ HTF_TESTS @-} Main2sub
--import   {-@ HTF_TESTS @-} Parser.LinesToParagraphs   -- pay attention to HTF_TESTS !
--import   {-@ HTF_TESTS @-} Parser.ProduceLit   -- pay attention to HTF_TESTS !
--import   {-@ HTF_TESTS @-} Parser.ProduceNLP   -- pay attention to HTF_TESTS !
-- makes call to NLP
--import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples   -- pay attention to HTF_TESTS !
-- import   {-@ HTF_TESTS @-} CoreNLP.Snippets2nt   -- pay attention to HTF_TESTS !
-- must have {-@ HTF_TESTS @-}

main =  do
    putStrLn "Lit Text Test.hs:\n"
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()
