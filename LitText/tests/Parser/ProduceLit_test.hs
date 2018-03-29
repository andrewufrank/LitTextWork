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
--import Uniform.Strings ((</>))  -- for PartURI
import Parser.ReadMarkupAB_test
--import Parser.ProduceLayout
--import BuchCode.BuchToken
--import BuchCode.MarkupText
--import Lines2para.Lines2para -- hiding ((</>))
--import Lines2para.HandleLayout
import           Text.Printf         (printf)
import           Uniform.Error           (errorT)
--import Uniform.FileIO
--import Path
import Uniform.Test.TestHarness
--import Data.RDF.FileTypes
import LitTypes.TextDescriptor hiding ((</>)) -- from Foundation
import BuchCode.BuchToken hiding ((</>), (<.>))
--import LitTypes.ServerNames  (rdfBase)  -- for test
--import Parser.ProduceLayout (buchURIx)
--import NLP2RDF.NLPvocabulary
import Parser.ProduceLit


test_werk = assertEqual (RDFtype "http://gerastree.at/lit_2014#Werk")
                (mkRDFtype ( "Werk"::Text ))

--produceLitTriples ::  TextDescriptor -> [TZ2] -> [Triple]  -- test CA -> H

instance ShowTestHarness TextDescriptor
instance ShowTestHarness [TZ2]
instance ShowTestHarness [Triple]

test_1CA_H = test2File progName "resultA1" "resultCA1" "resultH1" produceLitTriples
test_2CA_H = test2File progName "resultA2" "resultCA2" "resultH2" produceLitTriples
test_3CA_H = test2File progName "resultA3" "resultCA3" "resultH3" produceLitTriples
test_4CA_H = test2File progName "resultA4" "resultCA4" "resultH4" produceLitTriples
test_5CA_H = test2File progName "resultA5" "resultCA5" "resultH5" produceLitTriples
test_6CA_H = test2File progName "resultA6" "resultCA6" "resultH6" produceLitTriples
--test_7CA_H = test2File progName result7A "resultCA7" "resultH7" produceLitTriples
test_8CA_H = test2File progName "resultA8" "resultCA8" "resultH8" produceLitTriples
test_9CA_H = test2File progName "resultA9" "resultCA9" "resultH9" produceLitTriples
test_10CA_H = test2File progName "resultA10" "resultCA10" "resultH10" produceLitTriples
test_11CA_H = test2File progName "resultA11" "resultCA11" "resultH11" produceLitTriples
test_12CA_H = test2File progName "resultA12" "resultCA12" "resultH12" produceLitTriples


--writeLitTriples :: FilePath -> FilePath ->  IO ()
--writeLitTriples source dest   = do
--    testDataDir <- getAppUserDataDir "LitTextTest"  :: Path Abs Dir
--    let source2 =  addFileName testDataDir    source :: Path Abs File
--    let dest2 =  addFileName testDataDir     dest :: Path Abs File
--    runErr $ do
----        putIOwords ["writeLitTriples", "write the triples as nt"]
--        tripstext <- readFile2 source2
--        let trips = readNote "writeLitTriples" tripstext :: [Triple]
--        write6 dest2 ntFileTriples trips
--    assertBool True
--    return ()
--
--
--test_1H_K = writeLitTriples   "resultH1" "resultK1"
--test_2H_K = writeLitTriples   "resultH2" "resultK2"
--test_3H_K = writeLitTriples   "resultH3" "resultK3"
--test_4H_K = writeLitTriples   "resultH4" "resultK4"
--test_5H_K = writeLitTriples   "resultH5" "resultK5"
--test_6H_K = writeLitTriples   "resultH6" "resultK6"
--test_7H_K = writeLitTriples   "resultH7" "resultK7"
--test_8H_K = writeLitTriples   "resultH8" "resultK8"
--test_9H_K = writeLitTriples   "resultH9" "resultK9"
--test_10H_K = writeLitTriples   "resultH10" "resultK10"
--test_11H_K = writeLitTriples   "resultH11" "resultK11"
--test_12H_K = writeLitTriples   "resultH12" "resultK12"
------

