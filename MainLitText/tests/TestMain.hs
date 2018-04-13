-----------------------------------------------------------------------------
--
-- Module      :   for automatic test with MainLitText
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
--import {-@ HTF_TESTS @-} LitText.CoreNLP.Linear2Triple_test

--import {-@ HTF_TESTS @-} LitText.NLP2RDF.ProduceDocCallNLP_test
--import {-@ HTF_TESTS @-} LitText.NLP2RDF.ProduceNLPtriples_test

--import {-@ HTF_TESTS @-} LitText.Parser.ReadMarkupAB_test  -- > Ax and Bx

--import {-@ HTF_TESTS @-} LitText.Lines.MarkupText_test  -- B -> BAx (Text -> [TextZeile])
--import   {-@ HTF_TESTS @-} LitText.Lines.HandleLayout_test
--                                        -- [TextZeile] -> [TZ]  -- test BA -> BB
--import   {-@ HTF_TESTS @-} LitText.Lines.Lines2text_test  --   Text -> [TZ1]  -- test B -> C
--                        -- and [TZ] -> [TZ1]  -- test BB -> BC
--import   {-@ HTF_TESTS @-} LitText.Lines.Lines2para_test  -- [TZ1] -> [TZ2]
--                                    -- test C -> CA
--import   {-@ HTF_TESTS @-} LitText.Parser.ProduceLayout_test
--                --  TextDescriptor -> [TZ1] -> Text  -- test C -> J
--import   {-@ HTF_TESTS @-} LitText.Parser.ProduceLit_test
--            -- [TZ2] -> [Triple] CA -> Hx (triples) and H -> K (nt)
--import {-@ HTF_TESTS @-} LitText.Parser.FilterTextForNLP_test   -- CA -> DA [TZ2] -> [Snip]
--            -- DA is the same as D
--        -- and snip4test :: [TZ1] -> [Snip] C -> D
---------------- filters literal text, but not ideal snips yet
--
--import {-@ HTF_TESTS @-} LitText.Parser.FormNLPsnips_test
--    -- D -> DB formSnips :: [Snip] -> [Snip]
-------- form snips which go to NLP (optimal size)
--
import   {-@ HTF_TESTS @-} LitText.Parser.ProduceNLP_test
        -- DA -> L, calls to NLP, takes time

main =  do
    putStrLn "LitFoundation.hs:\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
