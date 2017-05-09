---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the test for the big stept
--     the common process to producing the lit and nlp triples
-- could test initially if the services (treetagger, fuseki, corenlp are available
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.Main2subTest (htf_thisModulesTests
        ) where

import           Test.Framework
import           Parser.Foundation        hiding ((</>))

import qualified          Parser.ReadMarkupAB as AB
        (textstate2Text, result1B, result2B, result3B, result4B)
import           Lines2para.Lines2para hiding ((</>))
import qualified Lines2para.Lines2paraTests as C
                ( result1BA, result2BA, result3BA, result4BA,
                result1C, result2C, result3C, result4C)
import           Store.Fuseki
import           Uniform.Error
import           Uniform.Strings
import qualified BuchCode.MarkupText as ABA
            (parseMarkup, result0B, result1B, result2B, result3B, result4B
            , result1BA, result2BA, result3BA, result4BA)


-- A_B test is in ReadMarkup
-- check that start of BuchCode is the same as end of ReadMarkupAB
test_ReadMarkup_export_BuchCode_1 = assertEqual AB.result1B ABA.result1B
test_ReadMarkup_export_BuchCode_2 = assertEqual AB.result2B ABA.result2B
test_ReadMarkup_export_BuchCode_3 = assertEqual AB.result3B ABA.result3B
test_ReadMarkup_export_BuchCode_4 = assertEqual AB.result4B ABA.result4B

-- B - C test:

parseToTZ ::   Text ->  [TZ]   -- Test B -> C
-- parse the text -- used for tests only (at the moment)
parseToTZ  = paragraphs2TZ . parseMarkup  -- test B -> BA -> C

test_1_B_C_parse_TZ_1 :: IO ()
-- convert a text with markup into a list ot coded text lines TZ
test_1_B_C_parse_TZ_1 = do
    putIOwords ["test_1_B_C_parse_TZ_1", "from result1B_textstate to result1C_tzResult1"]
    let t1 = parseToTZ AB.result1B
    assertEqual C.result1C t1

test_2_B_C_parse_TZ_1 :: IO ()
-- convert a text with markup into a list ot coded text lines TZ
test_2_B_C_parse_TZ_1 = do
    putIOwords ["test_2_B_C_parse_TZ_1", "from result2B_textstate to result2C_tzResult1"]
    let t1 = parseToTZ AB.result2B
    assertEqual C.result2C t1

test_3_B_C_parse_TZ_1 = do
    putIOwords ["test_3_B_C_parse_TZ_1", "from result3B_textstate to result3C_tzResult1"]
    let t1 = parseToTZ AB.result3B
    assertEqual C.result3C t1

test_4_B_C_parse_TZ_1 = do
    putIOwords ["test_4_B_C_parse_TZ_1", "from result3B_textstate to result4C_tzResult1"]
    let t1 = parseToTZ AB.result4B
    assertEqual C.result4C t1



--test_1_H_Z_storeTriplesFuseki = do
--    putIOwords ["test_storeTriplesFuseki: test the strorage of lit triples "] -- tripleResult]
--    t1 <- runErr $ storeTriplesFuseki result1A result1H_tripleResult1
--    putIOwords ["test_storeTriplesFuseki: result - should be Right \"\" : ", showT  t1]
----    putIOwords ["test_parseToTZ:  result ", show' t1]
--    assertEqual (Right "")  t1
