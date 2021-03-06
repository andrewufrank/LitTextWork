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
import Parser.FormNLPsnips
import Parser.FilterTextForNLP
import Parser.ProduceDocCallNLP
import Parser.ProduceNLPtriples hiding ((</>))
import Parser.CompleteSentence  (completeSentence, URI, serverBrest)
import          Data.RDF.FileTypes (ntFileTriples, ntFileTriplesGZip)
import Data.Maybe (catMaybes)  -- todo
-- for tests:
import Parser.ReadMarkupAB
import Parser.TextDescriptor -- (TextDescriptor(..), serverLoc, originalsDir)
import Uniform.FileIO (Path(..), Abs, File, TypedFiles5(..), resolveFile, Handle)

debugNLP1 = False

-- main export
--produceNLP :: Bool -> TextDescriptor ->  [TZ2] -> ErrIO () -- test C  -> X
---- produce the triples and store them in triple store,
---- first extract only the text TZ lines and convert the hyphenated texts
---- repeated for each paragraph
--produceNLP showXML textstate tzs = do
--    let     nlpTexts = prepareTZ4nlp tzs :: [Snip]
--            nlpTexts2 = formSnips nlpTexts :: [Snip]
--
--    foldM_ (produceOneSnip showXML ) textstate nlpTexts2
--    return ()
----produceNLP showXML textstate tzs = foldM_ (produceOneParaNLP showXML ) textstate tzs

produceNLP ::  Bool -> TextDescriptor ->  [TZ2] -> ErrIO () -- test C  -> X
produceNLP showXML textstate tzs =  do
    let     nlpTexts = prepareTZ4nlp tzs :: [Snip]
            snips = formSnips nlpTexts :: [Snip]
            debug = False
    triples :: [[Triple]] <-mapM (convertOneSnip2Triples debug showXML textstate) snips
--    let trips = readNote "writeLitTriples" tripstext :: [Triple]
--    write6 dest2 ntFileTriples trips
    ntz1 <- foldM writeHandleTriples textstate triples
--    putIOwords ["\n\nproduceOneParaNLP nlp triples ", "one snip done"
--            ,"snip size", showT $ tz3textLength snip
--            ,"from text", buchName textstate
--            ]

    return ()

--convertOneSnip2Triples :: Bool -> TextDescriptor -> Snip -> ErrIO [Triple]
---- calls nlp to convert to doc
--convertOneSnip2Triples showXML textstate snip = do
--    doc :: Doc0 postag <- snip2doc False showXML (nlpServer textstate) snip
--    let res =  processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (1, doc)
--    return res


produceNLPnotshow = produceNLP False

--test_1_BAE_XproduceNLPtriples :: IO ()
test_1_BAE_XproduceNLPtriples = testVar3FileIO result1A "resultBAE1" "resultX1" produceNLPnotshow
test_2_BAE_XproduceNLPtriples = testVar3FileIO result2A "resultBAE2" "resultX2" produceNLPnotshow
test_3_BAE_XproduceNLPtriples = testVar3FileIO result3A "resultBAE3" "resultX3" produceNLPnotshow
test_4_BAE_XproduceNLPtriples = testVar3FileIO result4A "resultBAE4" "resultX4" produceNLPnotshow
test_5_BAE_XproduceNLPtriples = testVar3FileIO result5A "resultBAE5" "resultX5" produceNLPnotshow
test_6_BAE_XproduceNLPtriples = testVar3FileIO result6A "resultBAE6" "resultX6" produceNLPnotshow
test_8_BAE_XproduceNLPtriples = testVar3FileIO result8A "resultBAE8" "resultX8" produceNLPnotshow
test_9_BAE_XproduceNLPtriples = testVar3FileIO result9A "resultBAE9" "resultX9" produceNLPnotshow
test_10_BAE_XproduceNLPtriples = testVar3FileIO result10A "resultBAE10" "resultX10" produceNLPnotshow
test_11_BAE_XproduceNLPtriples = testVar3FileIO result11A "resultBAE11" "resultX11" produceNLPnotshow
test_12_BAE_XproduceNLPtriples = testVar3FileIO result12A "resultBAE12" "resultX12" produceNLPnotshow
------ no result file is necessary, because result is zero
------ but results are found in LitTest/test
--



--produceOneSnip :: Bool -> TextDescriptor -> Snip -> ErrIO TextDescriptor
--produceOneSnip showXML textstate snip = do
----    (ntz,docs) :: (Snip,[Doc0]) <- convertTZ2nlp False showXML (nlpServer textstate) tzp  -- C -> E
--    doc :: Doc0 <- snip2doc False showXML (nlpServer textstate) snip
--    produceOneOneParaNLP  snip textstate doc
--
--produceOneOneParaNLP :: Snip -> TextDescriptor ->   Doc0  -> ErrIO TextDescriptor
--produceOneOneParaNLP snip textstate     doc0'   =   do  -- tz is Snip
--    let triples = convertOneSnip2Triples snip textstate doc0'
--    when debugNLP1 $
--                putIOwords ["\nproduceOneParaNLP read doc0", showT doc0', "\n"]
--    --    let buchuri = buchURIx textstate :: RDFsubj
--    when debugNLP1 $
--            putIOwords ["\n\nproduceOneParaNLP nlp triples "
--                , unlines' . map showT $ triples]
--    ntz1 <- writeHandleTriples textstate triples
--    putIOwords ["\n\nproduceOneParaNLP nlp triples ", "one snip done"
--            ,"snip size", showT $ tz3textLength snip
--            ,"from text", buchName textstate
--            ]
--    return ntz1

--convertOneSnip_Doc2Triples :: Snip -> TextDescriptor -> Doc0 -> [Triple]
---- convert a doc with snip to triples
---- textstate is used for the uid construction only
--convertOneSnip_Doc2Triples ntz textstate doc0' = processDoc0toTriples2 textstate (tz3lang ntz) (tz3para $ ntz) (1, doc0')


openHandleTriples  :: TextDescriptor -> ErrIO TextDescriptor
openHandleTriples textstate  = do
    let mhand = destHandle textstate
    case mhand of
        Nothing ->  do
--                putIOwords ["openHandleTriples", "to", showT $ destNT textstate]
                hand <- if gzipFlag textstate
                    then openHandle6 (destNT textstate) ntFileTriplesGZip
                    else openHandle6 (destNT textstate)  ntFileTriples
                return textstate{destHandle = Just hand}
            `catchError` \e -> do
                putIOwords ["openHandleTriples - error ", e ]
--                openHandleTriples2 textstate
                return textstate

        Just hand -> do
--             putIOwords ["openHandleTriples is open", "to", showT $ destNT textstate]
             return textstate

--openHandleTriples2  :: TextDescriptor -> ErrIO TextDescriptor


writeHandleTriples :: TextDescriptor -> [Triple] -> ErrIO TextDescriptor
writeHandleTriples  textstate tris = do
--                putIOwords ["writeHandleTriples"]
                textstate2 <- openHandleTriples textstate
                let hand = fromJustNote "writeHandleTriples" (destHandle textstate2)
                if gzipFlag textstate
                    then writeHandle6 hand ntFileTriplesGZip tris
                    else writeHandle6 hand ntFileTriples tris
                return textstate2

closeHandleTriples :: TextDescriptor ->  ErrIO TextDescriptor
closeHandleTriples textstate = do
                let hand = fromJustNote "closeHandleTriples" (destHandle textstate)
                if gzipFlag textstate
                    then closeHandle6 (destNT textstate) ntFileTriplesGZip hand
                    else closeHandle6 (destNT textstate) ntFileTriples hand
                let textstate2 = textstate{destHandle=Nothing}
                return textstate2





