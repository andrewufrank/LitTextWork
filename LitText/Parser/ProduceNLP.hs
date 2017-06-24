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

module Parser.ProduceNLP
    (module Parser.ProduceNLP
    ) where

import           Test.Framework
import Uniform.TestHarness
import Parser.ProduceDocCallNLP
import Parser.ProduceNLPtriples hiding ((</>))
import Parser.CompleteSentence  (completeSentence, URI, serverBrest)
import          Data.RDF.FileTypes (ntFileTriples)
import Data.Maybe (catMaybes)  -- todo
-- for tests:
import Parser.ReadMarkupAB
import Parser.Foundation
import Uniform.FileIO

debugNLP1 = False

-- main export
produceNLP :: TextState2 -> [TZ2] -> ErrIO () -- test C  -> X
-- produce the triples and store them in triple store,
-- first extract only the text TZ lines and convert the hyphenated texts
-- repeated for each paragraph
produceNLP textstate = mapM_ (produceOneParaNLP debugNLP1 textstate)

test_1_D_XproduceNLPtriples :: IO ()
test_1_D_XproduceNLPtriples = testVar3FileIO result1A "resultBAE1" "resultX1" produceNLP
test_2_D_XproduceNLPtriples = testVar3FileIO result2A "resultBAE2" "resultX2" produceNLP
test_3_D_XproduceNLPtriples = testVar3FileIO result3A "resultBAE3" "resultX3" produceNLP
test_4_D_XproduceNLPtriples = testVar3FileIO result4A "resultBAE4" "resultX4" produceNLP
test_5_D_XproduceNLPtriples = testVar3FileIO result5A "resultBAE5" "resultX5" produceNLP
test_6_D_XproduceNLPtriples = testVar3FileIO result6A "resultBAE6" "resultX6" produceNLP
test_8_D_XproduceNLPtriples = testVar3FileIO result8A "resultBAE8" "resultX8" produceNLP
---- no result file is necessary, because result is zero
---- but results are found in LitTest/test
--

testOP_E_F :: TextState2 -> [Maybe (NLPtext,Doc0)] -> ErrIO [(NLPtext,Doc0)]
testOP_E_F textstate  =  mapM  (completeSentencesInDoc debugNLP1 textstate) . catMaybes

test_1_E_F :: IO ()
test_1_E_F = testVar3FileIO result1A "resultE1" "resultF1" testOP_E_F
test_2_E_F = testVar3FileIO result2A "resultE2" "resultF2" testOP_E_F
test_3_E_F = testVar3FileIO result3A "resultE3" "resultF3" testOP_E_F
test_4_E_F = testVar3FileIO result4A "resultE4" "resultF4" testOP_E_F
test_5_E_F = testVar3FileIO result5A "resultE5" "resultF5" testOP_E_F
test_6_E_F = testVar3FileIO result6A "resultE6" "resultF6" testOP_E_F
test_7_E_F = testVar3FileIO result7A "resultE7" "resultF7" testOP_E_F
test_8_E_F = testVar3FileIO result8A "resultE8" "resultF8" testOP_E_F



completeSentencesInDoc :: Bool -> TextState2 -> (NLPtext, Doc0) -> ErrIO (NLPtext, Doc0)
-- complete the german sentences in the Doc (with lemmas
completeSentencesInDoc debugFlag textstate (ntz, doc0) = do
    let lang = tz3lang ntz
    let nlpserver = serverLoc textstate
    if lang == German
        then do
            let sents1 = docSents doc0
            sents2 <- mapM (completeSentence False nlpserver lang) sents1
            let doc0' = doc0{docSents = sents2}
            return (ntz, doc0')
        else return (ntz, doc0)


produceOneParaNLP :: Bool -> TextState2 -> TZ2 -> ErrIO ()
produceOneParaNLP showXML textstate tzp = do
    m1 <- convertTZ2nlp debugNLP1 showXML (serverLoc textstate) tzp  -- C -> E
    case m1 of
        Nothing -> return ()
        Just (ntz, doc0)  -> do  -- tz is NLPtext
            (ntz, doc0') <- completeSentencesInDoc debugNLP1 textstate (ntz, doc0)

            when debugNLP1 $
                    putIOwords ["\nproduceOneParaNLP read doc0", showT doc0', "\n"]
        --    let buchuri = buchURIx textstate :: RDFsubj

            let triples  = processDoc0toTriples2 textstate (ntz, doc0')  -- F -> G

            when debugNLP1 $
                putIOwords ["\n\nproduceOneParaNLP nlp triples "
                    , unlines' . map showT $ triples]
                    -- todo fileio add filepath to dir
            let newFileName =  (authorDir textstate)
                               ++ "/" ++ buchname textstate  ++ "." ++ ("nt"::FilePath) ::FilePath
            filenameRes :: Path Abs File <- resolveFile (originalsDir  textstate)
                               (newFileName::FilePath)
            let textstate2 = textstate{textfilename=filenameRes}
            appendTriples2file textstate2 triples
--            when debugNLP $ putIOwords ["produceOneParaNLP triples stored   "
--                        , showT . textfilename $ textstate, " \n", showT response ]
--            let response2 = response <>
--                        (showT . tlpara . tzloc $ tzp) <> "on page" <>
--                        (showT . tlpage . tzloc $ tzp)
            return ()

appendTriples2file :: TextState2 -> [Triple] -> ErrIO ()
appendTriples2file textstate tris = do
--    write6 (textfilename textstate)  ntFileTriples tris
    append6 (textfilename textstate)  ntFileTriples tris
    -- file must be deleted first!


writeTriples2file :: TextState2 -> [Triple] -> ErrIO ()
writeTriples2file textstate tris = do
--    write6 (textfilename textstate)  ntFileTriples tris
    write6 (textfilename textstate)  ntFileTriples tris
    -- file must be deleted first!


