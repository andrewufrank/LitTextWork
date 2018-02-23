 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceText
-- Copyright   :  andrew u frank -
--
-- | producing the triples for representing the literary text
-- mostly avoided in v2 which does only include what is actually used to classify texts
-- especially fables

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Parser.ProduceLit_test  where

import           Test.Framework
import           Data.Char               (toLower)
import           Data.Maybe               (isNothing)
--import           Data.RDF
--import           Data.RDF.Extension
--import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB_test
--import Parser.ProduceLayout
--import BuchCode.BuchToken
--import BuchCode.MarkupText
--import Lines2para.Lines2para -- hiding ((</>))
--import Lines2para.HandleLayout
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
import Uniform.TestHarness
--import Data.RDF.FileTypes
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import BuchCode.BuchToken hiding ((</>), (<.>))
import Producer.Servers  (rdfBase)  -- for test
--import Parser.ProduceLayout (buchURIx)
import Parser.NLPvocabulary
import Parser.ProduceLit


test_werk = assertEqual (RDFtype "http://gerastree.at/lit_2014#Werk")
                (mkRDFtype ( "Werk"::Text ))



test_1BAE_H = testVar3File result1A "resultBAE1" "resultH1" produceLitTriples
test_2BAE_H = testVar3File result2A "resultBAE2" "resultH2" produceLitTriples
test_3BAE_H = testVar3File result3A "resultBAE3" "resultH3" produceLitTriples
test_4BAE_H = testVar3File result4A "resultBAE4" "resultH4" produceLitTriples
test_5BAE_H = testVar3File result5A "resultBAE5" "resultH5" produceLitTriples
test_6BAE_H = testVar3File result6A "resultBAE6" "resultH6" produceLitTriples
--test_7BAE_H = testVar3File result7A "resultBAE7" "resultH7" produceLitTriples
test_8BAE_H = testVar3File result8A "resultBAE8" "resultH8" produceLitTriples
test_9BAE_H = testVar3File result9A "resultBAE9" "resultH9" produceLitTriples
test_10BAE_H = testVar3File result10A "resultBAE10" "resultH10" produceLitTriples
test_11BAE_H = testVar3File result11A "resultBAE11" "resultH11" produceLitTriples
test_12BAE_H = testVar3File result12A "resultBAE12" "resultH12" produceLitTriples


writeLitTriples :: FilePath -> FilePath ->  IO ()
writeLitTriples source dest   = do
    testDataDir <- getAppUserDataDir "LitTextTest" -- :: Path Abs Dir
    let source2 =  addFileName testDataDir    source :: Path Abs File
    let dest2 =  addFileName testDataDir     dest :: Path Abs File
    runErr $ do
--        putIOwords ["writeLitTriples", "write the triples as nt"]
        tripstext <- readFile2 source2
        let trips = readNote "writeLitTriples" tripstext :: [Triple]
        write6 dest2 ntFileTriples trips
    assertBool True
    return ()


test_1H_K = writeLitTriples   "resultH1" "resultK1"
test_2H_K = writeLitTriples   "resultH2" "resultK2"
test_3H_K = writeLitTriples   "resultH3" "resultK3"
test_4H_K = writeLitTriples   "resultH4" "resultK4"
test_5H_K = writeLitTriples   "resultH5" "resultK5"
test_6H_K = writeLitTriples   "resultH6" "resultK6"
test_7H_K = writeLitTriples   "resultH7" "resultK7"
test_8H_K = writeLitTriples   "resultH8" "resultK8"
test_9H_K = writeLitTriples   "resultH9" "resultK9"
test_10H_K = writeLitTriples   "resultH10" "resultK10"
test_11H_K = writeLitTriples   "resultH11" "resultK11"
test_12H_K = writeLitTriples   "resultH12" "resultK12"
--

