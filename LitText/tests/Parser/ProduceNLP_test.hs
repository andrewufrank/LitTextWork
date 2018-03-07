-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | the processing with NLP processors are in ProduceNLP
--
-- version 2 assumes that each paragraph is individually analyzed
--  for german - the lemma are determined for each sentence individually
-- using the tokenization from the coreNLP

-- later - open language changes inside paragraph :
-- snippets are pieces in one paragraph of one languageBreakCode
-- therefo~~~~~re the snippet id is paragraph id + count
--
-- the aggregation of small paragraphs to longer units to snaps will be
-- done later, which will require a snap unit consisting of serveral paragraphs

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLP_test where

import           Test.Framework
import Uniform.TestHarness

import          Data.RDFext.FileTypes -- (ntFileTriples, ntFileTriplesGZip,writeHandleTriples)
import Parser.ReadMarkupAB_test
import Parser.ProduceNLP
--import Uniform.Error
import Uniform.FileIO (getAppUserDataDir')



testOP_DA_L :: TextDescriptor -> [Snip]-> ErrIO [[Triple]]
testOP_DA_L textstate = mapM (convertOneSnip2Triples  [] textstate )

test_1_DA_L = testVar3FileIO result1A "resultDA1" "resultE1" testOP_DA_L
--test_2_DA_L = testVar3FileIO result2A "resultDA2" "resultE2" testOP_DA_L
--test_3_DA_L = testVar3FileIO result3A "resultDA3" "resultE3" testOP_DA_L
--test_4_DA_L = testVar3FileIO result4A "resultDA4" "resultE4" testOP_DA_L
--test_5_DA_L = testVar3FileIO result5A "resultDA5" "resultE5" testOP_DA_L  -- lafayette
--test_6_DA_L = testVar3FileIO result6A "resultDA6" "resultE6" testOP_DA_L
--test_8_DA_L = testVar3FileIO result8A "resultDA8" "resultE8" testOP_DA_L
----test_9_DA_L = testVar3FileIO result9A "resultDA9" "resultE9" testOP_DA_L  -- multiple language - not a single head
--test_10_DA_L = testVar3FileIO result10A "resultDA10" "resultE10" testOP_DA_L
--test_11_DA_L = testVar3FileIO result11A "resultDA11" "resultE11" testOP_DA_L
--test_12_DA_L = testVar3FileIO result12A "resultDA12" "resultE12" testOP_DA_L
--test_13_DA_L = testVar3FileIO result12A "resultDA12" "resultE12UD" testOP_DA_L

--produceNLPtest ::  TextDescriptor ->  [TZ2] -> ErrIO ()
produceNLPtest textstate tzs  = do
    trips <- produceNLPtriples [] textstate tzs
    let ntdescr = (ntdescriptor textstate) {gzipFlag = True}
    putIOwords ["produceNLPtest" , showT ntdescr ]
    bracketErrIO (do
                    putIOwords ["produceNLPtest open1"]
                    openHandleTriples ntdescr )
                (\h -> do closeHandleTriples ntdescr h
                          putIOwords ["produceNLPtest close" ]
                    )
                (\h -> do
                    putIOwords ["produceNLPtest write 1" ]
                    writeHandleTriples ntdescr h trips
                    putIOwords ["produceNLPtest write - close" ]  -- never reached?
                                -- the testing throws an exception
                                -- HUnit... but this is not caught by bracketErrIO
                    closeHandleTriples ntdescr h
                    )

    return ()
test_8_BAE_XproduceNLPtriples2 = do
    r <- runErr $ produceNLPtest2 result10A "resultE8" "resultX8"
    assertEqual "Right ()" (showT r)

--test_10_BAE_XproduceNLPtriples2 = do
--    r <- runErr $ produceNLPtest2 result10A "resultE10" "resultX10"
--    assertEqual "Right ()" (showT r)

produceNLPtest2::  TextDescriptor ->  FilePath -> FilePath -> ErrIO ()
produceNLPtest2 textstate startfile resfile  = do
    testDataDir <-   getAppUserDataDir' $ "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    f0 :: Text <- readFile2 fn0
    putIOwords ["test3 testVar3FileIO A", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    let trips = readNote "produceNLPtest2 read trips asdwer" . t2s $ f0
    putIOwords ["testVar3FileIO  trips",   showT trips]
--    trips <- produceNLPtriples textstate tzs
    let ntdescr = (ntdescriptor textstate) {gzipFlag = True}
    putIOwords ["produceNLPtest" , showT ntdescr ]
    bracketErrIO (do
                    putIOwords ["produceNLPtest open1"]
                    openHandleTriples ntdescr )
                (\h -> do closeHandleTriples ntdescr h
                          putIOwords ["produceNLPtest close" ]
                    )
                (\h -> do
                    putIOwords ["produceNLPtest write 1" ]
                    writeHandleTriples ntdescr h trips
--                    putIOwords ["produceNLPtest write - close" ]  -- never reached?
                                -- the testing throws an exception
                                -- HUnit... but this is not caught by bracketErrIO
--                    closeHandleTriples ntdescr h
                    )

    return ()

--test_1_BAE_XproduceNLPtriples :: IO ()
test_1_BAE_XproduceNLPtriples = testVar3FileIO result1A "resultBAE1" "resultX1" produceNLPtest
--test_2_BAE_XproduceNLPtriples = testVar3FileIO result2A "resultBAE2" "resultX2" produceNLPtest
--test_3_BAE_XproduceNLPtriples = testVar3FileIO result3A "resultBAE3" "resultX3" produceNLPtest
--test_4_BAE_XproduceNLPtriples = testVar3FileIO result4A "resultBAE4" "resultX4" produceNLPtest
--test_5_BAE_XproduceNLPtriples = testVar3FileIO result5A "resultBAE5" "resultX5" produceNLPtest
--test_6_BAE_XproduceNLPtriples = testVar3FileIO result6A "resultBAE6" "resultX6" produceNLPtest
--test_8_BAE_XproduceNLPtriples = testVar3FileIO result8A "resultBAE8" "resultX8" produceNLPtest
--test_9_BAE_XproduceNLPtriples = testVar3FileIO result9A "resultBAE9" "resultX9" produceNLPtest
--test_10_BAE_XproduceNLPtriples = testVar3FileIO result10A "resultBAE10" "resultX10" produceNLPtest
--test_11_BAE_XproduceNLPtriples = testVar3FileIO result11A "resultBAE11" "resultX11" produceNLPtest
--test_12_BAE_XproduceNLPtriples = testVar3FileIO result12A "resultBAE12" "resultX12" produceNLPtest
------ no result file is necessary, because result is zero
------ but results are found in LitTest/test
--

test_produceNLPtest    = do
    r <- runErr $
        do
            let trips = []
            let dir = makeAbsFile "/home/frank/test_produceNLP"
            let ntdescr = NTdescriptor {gzipFlag = True, destNT = dir }
            putIOwords ["produceNLPtest" , showT ntdescr ]
            r <- bracketErrIO (do
                            putIOwords ["produceNLPtest open"]
                            openHandleTriples ntdescr )
                        (\h -> do closeHandleTriples ntdescr h
                                  putIOwords ["produceNLPtest close" ]
                            )
                        (\h -> do
                            putIOwords ["produceNLPtest write" ]
                            writeHandleTriples ntdescr h trips
                            )
            return r
    assertEqual (Right ()) r




