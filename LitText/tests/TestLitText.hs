---------------------------------------------------------------------------
--
-- Module      :   a test for automatic check of treetagger call
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import  Uniform.Strings
import  Test.Framework

-- note BAE == C

-- for layout and lit triples :
--
--import {-@ HTF_TESTS @-} Parser.ReadMarkupAB_test  -- > Bx
--
import {-@ HTF_TESTS @-} BuchCode.MarkupText_test  -- > BAx
--import   {-@ HTF_TESTS @-} Lines2para.HandleLayout_test -- > BACx
--import   {-@ HTF_TESTS @-} Lines2para.Lines2text_test  --   B -> C

--import   {-@ HTF_TESTS @-} Parser.ProduceLayout_test  -- > BC -> J
--------    -- not enough memory on oporto
-------- if the files are not already correct in .littest
-------- problem is in the comparing when error
--import   {-@ HTF_TESTS @-} Lines2para.Lines2para_test -- BADx > BAEx
--import   {-@ HTF_TESTS @-} Parser.ProduceLit_test  -- BAEx -> Hx (triples)
----                -- and Hx -> Kx  (.nt)
------------------
-------- for nlp:
----
--------
--import {-@ HTF_TESTS @-} Parser.FilterTextForNLP_test   -- BAE=C -> D
---------------- filters literal text

----import {-@ HTF_TESTS @-} Parser.ProduceNLPtriples_test  --  E -> G and G -> L
--   does not yet work (ambigous var)

--import   {-@ HTF_TESTS @-} Parser.ProduceNLP_test  -- BAE=C -> D and BAE -> X1  -- overall test, run at end
--            --repeats calls to nlp
--            -- files go to
--import {-@ HTF_TESTS @-} Parser.FormNLPsnips_test   -- D -> DA
-------- form snips which go to NLP
----
----import   {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test   -- DA -> L, calls to NLP, takes time
--    -- calls produceNLPtriples
--------------    -- makes result  E
------
--------------                -- tests only the production of the doc files
----------                      all tests above can be switched off
------
------import   {-@ HTF_TESTS @-} Parser.CompleteSentence  -- no test
---------------------- calls 17701
------------------
------------import   {-@ HTF_TESTS @-} Parser.ProduceNLPtriples_test  -- removed -- E (doc) -> G  and L (triples)
------
------import   {-@ HTF_TESTS @-} CoreNLP.CoreNLPxml_test
------ tests tp check codes
------import   {-@ HTF_TESTS @-}  CoreNLP.POScodesFrench_test
-------- old
------import {-@ HTF_TESTS @-} Processor.CheckServer_tests
----------import   {-@ HTF_TESTS @-} Parser.ConvertTaggerOutput_test
------------ no test for NLPvocabulary
--------
--------
---------- makes call to NLP
----------import {-@ HTF_TESTS @-} Main2sub_test
--------import {-@ HTF_TESTS @-} Parser.Main2subTest_test
--------import  {-@ HTF_TESTS @-} Processor.ProcessAll_test

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "Lit Text Test.hs end ------------- \n"
    return ()


