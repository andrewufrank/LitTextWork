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

module LitText.Parser.ProduceNLP_test where

import           Test.Framework
import Uniform.Test.TestHarness

import LitText.Parser.ReadMarkupAB_test
import LitText.Parser.ProduceNLP
import Uniform.FileIO (getAppUserDataDir')

--progName = "tests"
instance ShowTestHarness [[Triple]]
instance ShowTestHarness [Snip]
--instance ShowTestHarness [TZ2]
--instance ShowTestHarness TextDescriptor


testOP_DA_E :: TextDescriptor -> [Snip]-> ErrIO [[Triple]]
testOP_DA_E textstate =  mapM (convertOneSnip2Triples  zero textstate )
          --   fmap concat .

test_1_DA_E = testVar1FileIO progName result1A "resultDA1" "resultE1" testOP_DA_E
test_2_DA_E = testVar1FileIO progName result2A "resultDA2" "resultE2" testOP_DA_E
test_3_DA_E = testVar1FileIO progName result3A "resultDA3" "resultE3" testOP_DA_E
test_4_DA_E = testVar1FileIO progName result4A "resultDA4" "resultE4" testOP_DA_E
test_5_DA_E = testVar1FileIO progName result5A "resultDA5" "resultE5" testOP_DA_E
            -- lafayette
test_6_DA_E = testVar1FileIO progName result6A "resultDA6" "resultE6" testOP_DA_E
test_8_DA_E = testVar1FileIO progName result8A "resultDA8" "resultE8" testOP_DA_E
test_9_DA_E = testVar1FileIO progName result9A "resultDA9" "resultE9" testOP_DA_E
            -- multiple language - not a single head
test_10_DA_E = testVar1FileIO progName result10A "resultDA10" "resultE10" testOP_DA_E
test_11_DA_E = testVar1FileIO progName result11A "resultDA11" "resultE11" testOP_DA_E
test_12_DA_E = testVar1FileIO progName result12A "resultDA12" "resultE12" testOP_DA_E
test_13_DA_E = testVar1FileIO progName result12A "resultDA12" "resultE12UD" testOP_DA_E

testOP_DA_EZ :: TextDescriptor -> [Snip]-> [Snip]
testOP_DA_EZ textstate =  map (convertOneSnip2TriplesX  [] textstate )
          --   fmap concat .

test_1_DA_EZ = testVar1File progName result1A "resultDA1" "resultEZ1" testOP_DA_EZ
test_2_DA_EZ = testVar1File progName result2A "resultDA2" "resultEZ2" testOP_DA_EZ
test_3_DA_EZ = testVar1File progName result3A "resultDA3" "resultEZ3" testOP_DA_EZ
test_4_DA_EZ = testVar1File progName result4A "resultDA4" "resultEZ4" testOP_DA_EZ
test_5_DA_EZ = testVar1File progName result5A "resultDA5" "resultEZ5" testOP_DA_EZ
            -- lafayette
test_6_DA_EZ = testVar1File progName result6A "resultDA6" "resultEZ6" testOP_DA_EZ
test_8_DA_EZ = testVar1File progName result8A "resultDA8" "resultEZ8" testOP_DA_EZ
test_9_DA_EZ = testVar1File progName result9A "resultDA9" "resultEZ9" testOP_DA_EZ
            -- multiple language - not a single head
test_10_DA_EZ = testVar1File progName result10A "resultDA10" "resultEZ10" testOP_DA_EZ
test_11_DA_EZ = testVar1File progName result11A "resultDA11" "resultEZ11" testOP_DA_EZ
test_12_DA_EZ = testVar1File progName result12A "resultDA12" "resultEZ12" testOP_DA_EZ
test_13_DA_EZ = testVar1File progName result12A "resultDA12" "resultEZ12UD" testOP_DA_EZ

produceNLPtest ::  TextDescriptor ->  [Snip] -> ErrIO Text
produceNLPtest textstate snips  = do
    trips <- produceNLPtriples zero textstate snips
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

    return "OK"

test_8_DA_X_produceNLPtriples2 = testVar1FileIO progName result8A "resultDA8" "resultX8"
            produceNLPtest

test_10_DA_X_produceNLPtriples2 = testVar1FileIO progName result10A "resultDA10" "resultX10"
            produceNLPtest
--    r <- runErr $ produceNLPtest2 result10A "resultDA10" "resultX10"
--    assertEqual "Right ()" (showT r)

produceNLPtest2::  TextDescriptor ->  FilePath -> FilePath -> ErrIO ()
produceNLPtest2 textstate startfile resfile  = do
    testDataDir <-   getAppUserDataDir' $ "LitTextTest"
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    f0 :: Text <- readFile2 fn0
    putIOwords ["test3 testVar1FileIO progName A", "resultFile:", s2t resfile, "inputFile:", showT fn0]
    let trips = readNote "produceNLPtest2 read trips asdwer" . t2s $ f0
    putIOwords ["testVar1FileIO progName  trips",   showT trips]
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


-- (base -> a-> ErrIO  b)
produceNLPtest4::  TextDescriptor ->  [[Triple]] ->   ErrIO  Text
produceNLPtest4 textstate trips2  = do
--    testDataDir <-   getAppUserDataDir' $ "LitTextTest"
--    let fn0 =  testDataDir   </> startfile :: Path Abs File
--    f0 :: Text <- readFile2 fn0
--    putIOwords ["test3 testVar1FileIO progName A", "resultFile:", s2t resfile, "inputFile:", showT fn0]
--    let trips = readNote "produceNLPtest2 read trips asdwer" . t2s $ f0
    putIOwords ["testVar1FileIO progName  trips",   showT trips2]
    let trips = concat trips2
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

    return "produceNLPtriples4 ok"

--test_1_E_XproduceNLPtriples :: IO ()
test_1_E_XproduceNLPtriples = testVar1FileIO progName result1A "resultE1" "resultX1"
             produceNLPtest4
test_2_E_XproduceNLPtriples = testVar1FileIO progName result2A "resultE2" "resultX2" produceNLPtest4
test_3_E_XproduceNLPtriples = testVar1FileIO progName result3A "resultE3" "resultX3" produceNLPtest4
test_4_E_XproduceNLPtriples = testVar1FileIO progName result4A "resultE4" "resultX4" produceNLPtest4
test_5_E_XproduceNLPtriples = testVar1FileIO progName result5A "resultE5" "resultX5" produceNLPtest4
test_6_E_XproduceNLPtriples = testVar1FileIO progName result6A "resultE6" "resultX6" produceNLPtest4
test_8_E_XproduceNLPtriples = testVar1FileIO progName result8A "resultE8" "resultX8" produceNLPtest4
test_9_E_XproduceNLPtriples = testVar1FileIO progName result9A "resultE9" "resultX9" produceNLPtest4
test_10_E_XproduceNLPtriples = testVar1FileIO progName result10A "resultE10" "resultX10" produceNLPtest4
test_11_E_XproduceNLPtriples = testVar1FileIO progName result11A "resultE11" "resultX11" produceNLPtest4
test_12_E_XproduceNLPtriples = testVar1FileIO progName result12A "resultE12" "resultX12" produceNLPtest4
------ no result file is necessary, because result is zero
------ but results are found in LitTest/test
--

test_produceNLPtest :: IO ()
test_produceNLPtest    = do
    r <- runErr $
        do
            let trips = []
            let dir = makeAbsFile "/home/frank/.tests/X1"
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




