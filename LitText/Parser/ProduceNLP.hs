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
import Parser.TextDescriptor -- (TextDescriptor(..), serverLoc, originalsDir)
import Uniform.FileIO (Path(..), Abs, File, TypedFiles5(..), resolveFile, Handle)

debugNLP1 = False

-- main export
produceNLP :: Bool -> TextDescriptor ->  [TZ2] -> ErrIO () -- test C  -> X
-- produce the triples and store them in triple store,
-- first extract only the text TZ lines and convert the hyphenated texts
-- repeated for each paragraph
produceNLP showXML textstate tzs = mapM_ (produceOneParaNLP showXML textstate) tzs

produceNLPnotshow = produceNLP False

test_1_BAE_XproduceNLPtriples :: IO ()
test_1_BAE_XproduceNLPtriples = testVar3FileIO result1A "resultBAE1" "resultX1" produceNLPnotshow
test_2_BAE_XproduceNLPtriples = testVar3FileIO result2A "resultBAE2" "resultX2" produceNLPnotshow
test_3_BAE_XproduceNLPtriples = testVar3FileIO result3A "resultBAE3" "resultX3" produceNLPnotshow
test_4_BAE_XproduceNLPtriples = testVar3FileIO result4A "resultBAE4" "resultX4" produceNLPnotshow
test_5_BAE_XproduceNLPtriples = testVar3FileIO result5A "resultBAE5" "resultX5" produceNLPnotshow
test_6_BAE_XproduceNLPtriples = testVar3FileIO result6A "resultBAE6" "resultX6" produceNLPnotshow
test_8_BAE_XproduceNLPtriples = testVar3FileIO result8A "resultBAE8" "resultX8" produceNLPnotshow
test_9_BAE_XproduceNLPtriples = testVar3FileIO result10A "resultBAE9" "resultX9" produceNLPnotshow
test_10_BAE_XproduceNLPtriples = testVar3FileIO result10A "resultBAE10" "resultX10" produceNLPnotshow
------ no result file is necessary, because result is zero
------ but results are found in LitTest/test
--

-- production of F to be used later
testOP_E_F :: LanguageCode -> TextDescriptor -> [(NLPtext,[Doc0])] -> ErrIO [ Doc0 ]
testOP_E_F lang textstate inp =
        mapM  (completeSentencesInDoc False textstate lang)  (concat $ map snd inp)
        -- does produce empty sets ...
        -- but is already wrong?
----
----
--test_1_E_F :: IO ()
test_1_E_F = testVar3FileIO result1A "resultE1" "resultF1" (testOP_E_F German)
test_2_E_F = testVar3FileIO result2A "resultE2" "resultF2" (testOP_E_F German)
test_3_E_F = testVar3FileIO result3A "resultE3" "resultF3" (testOP_E_F German)
test_4_E_F = testVar3FileIO result4A "resultE4" "resultF4" (testOP_E_F German)
test_5_E_F = testVar3FileIO result5A "resultE5" "resultF5" (testOP_E_F English)
test_6_E_F = testVar3FileIO result6A "resultE6" "resultF6" (testOP_E_F English)
----test_7_E_F = testVar3FileIO result7A "resultE7" "resultF7" (testOP_E_F English)
test_8_E_F = testVar3FileIO result8A "resultE8" "resultF8" (testOP_E_F English)
test_9_E_F = testVar3FileIO result9A "resultE9" "resultF9" (testOP_E_F German)
test_10_E_F = testVar3FileIO result10A "resultE10" "resultF10" (testOP_E_F English)
------ 9 german
------ 10 english


completeSentencesInDoc :: Bool -> TextDescriptor -> LanguageCode -> ( Doc0)
                        -> ErrIO ( Doc0)
-- complete the german sentences in the Doc (with lemmas
completeSentencesInDoc debugFlag textstate lang ( doc0) = do
--    let lang = tz3lang ntz
    let nlpserver = nlpServer textstate
    if lang == German
        then do
            let sents1 = docSents doc0
            sents2 <- mapM (completeSentence False nlpserver lang) sents1
            let doc0' = doc0{docSents = sents2}
            return ( doc0')
        else return (  doc0)


produceOneParaNLP :: Bool -> TextDescriptor -> TZ2 -> ErrIO ()
produceOneParaNLP showXML textstate tzp = do
    (ntz,docs) :: (NLPtext,[Doc0]) <- convertTZ2nlp False showXML (nlpServer textstate) tzp  -- C -> E
    mapM_  (produceOneOneParaNLP textstate ntz) (zip [1..] docs )

produceOneOneParaNLP :: TextDescriptor -> NLPtext -> (Int, Doc0)  -> ErrIO ()
produceOneOneParaNLP textstate  ntz (snipnr, doc0)  =   do  -- tz is NLPtext
        let lang = tz3lang ntz
        let paranr = tz3para $ ntz
        ( doc0') <- completeSentencesInDoc debugNLP1 textstate lang ( doc0)

        when debugNLP1 $
                putIOwords ["\nproduceOneParaNLP read doc0", showT doc0', "\n"]
    --    let buchuri = buchURIx textstate :: RDFsubj

        let triples  = processDoc0toTriples2 textstate lang paranr (snipnr, doc0')   -- F -> G

        when debugNLP1 $
            putIOwords ["\n\nproduceOneParaNLP nlp triples "
                , unlines' . map showT $ triples]
                -- todo fileio add filepath to dir
--        let newFileName =  (authorDir textstate)
--                           ++ "/" ++ buchname textstate  ++ "." ++ ("nt"::FilePath) ::FilePath
--        filenameRes :: Path Abs File <- resolveFile (originalsDir  textstate)
--                           (newFileName::FilePath)
--        let textstate2 = textstate{textfilename=filenameRes}
        writeHandleTriples textstate triples
--        appendTriples2file textstate  triples
--            when debugNLP $ putIOwords ["produceOneParaNLP triples stored   "
--                        , showT . textfilename $ textstate, " \n", showT response ]
--            let response2 = response <>
--                        (showT . tlpara . tzloc $ tzp) <> "on page" <>
--                        (showT . tlpage . tzloc $ tzp)
        return ()


--appendTriples2file :: TextDescriptor -> [Triple] -> ErrIO ()
--appendTriples2file textstate tris = do
--    append6 (destNT textstate)  ntFileTriples tris
--    -- file must be deleted first!
--
--
--writeTriples2file :: TextDescriptor -> [Triple] -> ErrIO ()
--writeTriples2file textstate tris = do
--    write6 (destNT textstate)  ntFileTriples tris
--    -- this deletes previous file

openHandleTriples  :: TextDescriptor -> ErrIO TextDescriptor
openHandleTriples textstate  = do
    let mhand = destHandle textstate
    case mhand of
        Nothing ->  do
            hand <- openHandle6 (destNT textstate)  ntFileTriples
            return textstate{destHandle = Just hand}
        Just hand ->  return textstate

writeHandleTriples :: TextDescriptor -> [Triple] -> ErrIO TextDescriptor
writeHandleTriples textstate tris = do
    let mhand = destHandle textstate
    case mhand of
        Nothing -> do
            textstate2 <- openHandleTriples textstate
            writeHandleTriples textstate2 tris
            return textstate2
        Just hand -> do
                writeHandle6 hand ntFileTriples tris
                return textstate






