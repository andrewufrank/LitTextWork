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

-- for tests:
import Parser.ReadMarkupAB
import Uniform.FileIO

debugNLP1 = False

-- main export
produceNLP :: TextState2 -> [TZ2] -> ErrIO () -- test C -> D -> X
-- produce the triples and store them in triple store,
-- first extract only the text TZ lines and convert the hyphenated texts
-- repeated for each paragraph
produceNLP textstate = mapM_ (produceOneParaNLP debugNLP1 textstate)

        -- prepareTZ4nlp is in ProduceDocCallNLP and converts tz2 to nlptext

test_1_D_XproduceNLPtriples :: IO ()
test_1_D_XproduceNLPtriples = testVar3FileIO result1A "resultBAE1" "resultX1" produceNLP
test_2_D_XproduceNLPtriples = testVar3FileIO result2A "resultBAE2" "resultX2" produceNLP
test_3_D_XproduceNLPtriples = testVar3FileIO result3A "resultBAE3" "resultX3" produceNLP
test_4_D_XproduceNLPtriples = testVar3FileIO result4A "resultBAE4" "resultX4" produceNLP
test_5_D_XproduceNLPtriples = testVar3FileIO result5A "resultBAE5" "resultX5" produceNLP
test_6_D_XproduceNLPtriples = testVar3FileIO result6A "resultBAE6" "resultX6" produceNLP
-- no result file is necessary, because result is zero
--


produceOneParaNLP :: Bool -> TextState2 -> TZ2 -> ErrIO ()
produceOneParaNLP showXML textstate tzp = do
    m1 <- convertTZ2nlp debugNLP1 showXML (serverLoc textstate) tzp  -- C -> E
    case m1 of
        Nothing -> return ()
        Just (tz, doc0)  -> do
            let lang = tz2lang tzp
            let sents1 = docSents doc0
            let nlpserver = serverLoc textstate

            sents2 <- if lang == German
                    then mapM (completeSentence False nlpserver lang) sents1  -- F -> G
                    else return sents1

            let doc0' = doc0{docSents = sents2}

            when debugNLP1 $
                    putIOwords ["\nproduceOneParaNLP read doc0", showT doc0', "\n"]
        --    let buchuri = buchURIx textstate :: RDFsubj
            let triples  = processDoc0toTriples2 textstate tzp doc0'  -- G -> H

            when debugNLP1 $
                putIOwords ["\n\nproduceOneParaNLP nlp triples "
                    , unlines' . map showT $ triples]
                    -- todo fileio add filepath to dir
            let newFileName =  (authorDir textstate)
                               ++ "/" ++ buchname textstate  ++ "." ++ ("nt"::FilePath) ::FilePath
            filenameRes :: Path Abs File <- resolveFile (originalsDir  textstate)
                               (newFileName::FilePath)
            let textstate2 = textstate{textfilename=filenameRes}
            writeTriples2file textstate2 triples
--            when debugNLP $ putIOwords ["produceOneParaNLP triples stored   "
--                        , showT . textfilename $ textstate, " \n", showT response ]
--            let response2 = response <>
--                        (showT . tlpara . tzloc $ tzp) <> "on page" <>
--                        (showT . tlpage . tzloc $ tzp)
            return ()

writeTriples2file :: TextState2 -> [Triple] -> ErrIO ()
writeTriples2file textstate tris = do
--    write6 (textfilename textstate)  ntFileTriples tris
    append6 (textfilename textstate)  ntFileTriples tris
    -- file must be deleted first!

    -- putIOwords ["storeTriplesFuseki", "endpoint", endpoint textstate]
--    insertTriplesIntoGraph fusekiServer (endpoint textstate)
--            tris  (Just (gerastreeURI </> graph textstate ))

--storeTriplesFuseki textstate tris = do
--    -- putIOwords ["storeTriplesFuseki", "endpoint", endpoint textstate]
--    insertTriplesIntoGraph fusekiServer (endpoint textstate)
--            tris  (Just (gerastreeURI </> graph textstate ))


--rightNote :: Text ->  ErrOrVal a -> a
--rightNote msg (Right a) = a
--rightNote msg (Left t) = errorT [ msg, t]

completeOneDoc :: URI -> LanguageCode -> Doc0  -> ErrIO Doc0  -- F -> G
completeOneDoc serverloc lang doc = do
    let s1 = docSents doc
    s2 <- mapM (completeSentence False serverloc lang) s1
    return (doc {docSents = s2})


