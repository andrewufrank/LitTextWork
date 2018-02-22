-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | produce the triples for the NLP part
-- the analysis is completed and stored in doc0
-- this is only producing the triples, converted from doc0

-- the snip has an id (which is the paraid of the first paragraph
-- snips are not separated in paragraphs but the sentences are part of the snip and the snip part of the book



-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLPtriples_test  where

import           Test.Framework
import Uniform.TestHarness (testVar3File)
import CoreNLP.Defs0
import CoreNLP.NERcodes
import Parser.TextDescriptor
import NLP.Types.Tags
import Parser.NLPvocabulary  -- from Foundation
import Parser.LanguageTypedText
import Data.List (partition)
-- import Parser.ReadMarkupAB -- is in LitText, which is above NLPservices
import Parser.ProduceNLPtriples


-- cannot be used, as result1A is in LitText, which is above
--testOP_E_G :: (Show postag,  POStags postag) => TextDescriptor -> [Doc0 postag] ->  [Triple]
--testOP_E_G textstate docs  = concat
--        . map (processDoc0toTriples2 textstate NoLanguage (ParaNum 99))
--        $ (zip (map SnipID [1 ..] docs)
-- here missing the values for language and paranr
-- fake paranr 99 should be ok for test
--
--test_1_E_G :: IO ()
--test_1_E_G =  testVar3File result1A "resultE1" "resultG1" testOP_E_G
--test_2_E_G =  testVar3File result2A "resultE2" "resultG2" testOP_E_G
--test_3_E_G =  testVar3File result3A "resultE3" "resultG3" testOP_E_G
--test_4_E_G =  testVar3File result4A "resultE4" "resultG4" testOP_E_G
--test_5_E_G =  testVar3File result5A "resultE5" "resultG5" testOP_E_G
--test_6_E_G = testVar3File result6A "resultE6" "resultG6" testOP_E_G
--test_7_E_G = testVar3File result6A "resultE7" "resultG7" testOP_E_G
--test_8_E_G = testVar3File result8A "resultE8" "resultG8" testOP_E_G
--test_9_E_G = testVar3File result9A "resultE9" "resultG9" testOP_E_G
--test_10_E_G = testVar3File result10A "resultE10" "resultG10" testOP_E_G
--test_11_E_G = testVar3File result11A "resultE11" "resultG11" testOP_E_G
--test_12_E_G = testVar3File result12A "resultE12" "resultG12" testOP_E_G
------ issue with ambiguity in posttag (Enn has no postag and is not Doc0)
----
--test_1G_L = writeLitTriples   "resultG1" "resultL1"
--test_2G_L = writeLitTriples   "resultG2" "resultL2"
--test_3G_L = writeLitTriples   "resultG3" "resultL3"
--test_4G_L = writeLitTriples   "resultG4" "resultL4"
--test_5G_L = writeLitTriples   "resultG5" "resultL5"
--test_6G_L = writeLitTriples   "resultG6" "resultL6"
--test_7G_L = writeLitTriples   "resultG7" "resultL7"
--test_8G_L = writeLitTriples   "resultG8" "resultL8"
--test_9G_L = writeLitTriples   "resultG9" "resultL9"
--test_10G_L = writeLitTriples   "resultG10" "resultL10"
--test_11G_L = writeLitTriples   "resultG11" "resultL11"
--test_12G_L = writeLitTriples   "resultG12" "resultL12"
