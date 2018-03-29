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
--import {-@ HTF_TESTS @-} Parser.ReadMarkupAB_test  -- > Ax and Bx
------
--import {-@ HTF_TESTS @-} Lines2para.MarkupText_test  -- B -> BAx (Text -> [TextZeile])
--import   {-@ HTF_TESTS @-} Lines2para.HandleLayout_test
--                                        -- [TextZeile] -> [TZ]  -- test BA -> BB
--
--import   {-@ HTF_TESTS @-} Lines2para.Lines2text_test  --   Text -> [TZ1]  -- test B -> C
--                        -- and [TZ] -> [TZ1]  -- test BB -> BC

--import   {-@ HTF_TESTS @-} Lines2para.Lines2para_test  -- [TZ1] -> [TZ2]
--                                    -- test C -> CA

--import   {-@ HTF_TESTS @-} Parser.ProduceLayout_test
--                --  TextDescriptor -> [TZ1] -> Text  -- test C -> J
--
--import   {-@ HTF_TESTS @-} Parser.ProduceLit_test
            -- [TZ2] -> [Triple] CA -> Hx (triples) and H -> K (nt)
--
------
--import {-@ HTF_TESTS @-} Parser.FilterTextForNLP_test   -- CA -> DA [TZ2] -> [Snip]
------            -- DA is the same as D
------        -- and snip4test :: [TZ1] -> [Snip] C -> D
---------------------- filters literal text, but not ideal snips yet
------
--import {-@ HTF_TESTS @-} Parser.FormNLPsnips_test   -- D -> DB formSnips :: [Snip] -> [Snip]
-------------- form snips which go to NLP (optimal size)
----
import   {-@ HTF_TESTS @-} Parser.ProduceNLP_test   -- DA -> L, calls to NLP, takes time




------import   {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP_test   -- DA -> L, calls to NLP, takes time
    -- calls produceNLPtriples
--------------    -- makes result  E
------
--------------                -- tests only the production of the doc files
----------                      all tests above can be switched off





main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "Lit Text Test.hs end ------------- \n"
    return ()


