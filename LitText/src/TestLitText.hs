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
import {-@ HTF_TESTS @-} Parser.ReadMarkupAB  -- > Bx
import {-@ HTF_TESTS @-} BuchCode.MarkupText  -- > BAx
import   {-@ HTF_TESTS @-} Lines2para.HandleLayout -- > BACx
import   {-@ HTF_TESTS @-} Lines2para.Lines2ignore  -- > BADx
import   {-@ HTF_TESTS @-} Parser.ProduceLayout  -- not enough memory on oporto
-- if the files are not already correct in .littest
-- problem is in the comparing when error
import   {-@ HTF_TESTS @-} Lines2para.Lines2para -- > BAEx
import   {-@ HTF_TESTS @-} Parser.ProduceLit  -- > Hx
--------
import   {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP   -- calls to NLP, takes time
--    -- makes result D and E
--                -- tests only the production of the doc files
import   {-@ HTF_TESTS @-} Parser.CompleteSentence
-------- calls 17701
import   {-@ HTF_TESTS @-} Parser.ProduceNLP -- > BAEx --> F
----     and   E -> F
------ first calls the nlp again, takes time, result goes in .nt in test
--
import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples  -- F -> G
--
---- old
------import {-@ HTF_TESTS @-} Processor.CheckServers
------import   {-@ HTF_TESTS @-} Parser.ConvertTaggerOutput
-------- no test for NLPvocabulary
----
----
------ makes call to NLP
------import {-@ HTF_TESTS @-} Main2sub
----import {-@ HTF_TESTS @-} Parser.Main2subTest
----import  {-@ HTF_TESTS @-} Processor.ProcessAll

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "Lit Text Test.hs end ------------- \n"
    return ()


