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

import Parser.Foundation  hiding ((<|>),(</>), (<.>)) -- for TZ
import Parser.ProduceDocCallNLP
import Parser.ProduceNLPtriples
import Uniform.Error   -- For ErrOrVal
import           Data.RDF   -- should all be imported from extension
import          Data.RDF.FileTypes
import Lines2para.Lines2para hiding ((<|>),(</>), (<.>))
import           Parser.NLPvocabulary hiding ((<|>),(</>), (<.>))
import           Uniform.Strings              hiding ((<|>))
import Uniform.FileIO hiding ((</>), (<.>))
import Parser.CompleteSentence -- (completeSentence)
-- for tests
import Parser.ReadMarkupAB -- (result1A)
import Data.Either

debugNLP1 = False

-- main export
produceNLPtriples :: TextState2 -> [TZ2] -> ErrIO () -- test C -> D -> X
-- produce the triples and store them in triple store,
-- first extract only the text TZ lines and convert the hyphenated texts
-- repeated for each paragraph
produceNLPtriples textstate = mapM_ (produceOneParaNLP debugNLP1 textstate)

        -- prepareTZ4nlp is in ProduceDocCallNLP and converts tz2 to nlptext

test_1_D_XproduceNLPtriples =  do   -- test C -> H
    putIOwords ["produceNLPtriples:  C=BAE -> H  "] -- , showT tzResult]
    t1 <-   runErr $ produceNLPtriples  result1A result1BAE
--    putIOwords ["produceNLPtriples: result (for next) ", s2t $ show t1]
--    putIOwords ["produceNLPtriples:  result ", showT t1]
    assertEqual (Right ())  t1

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

test_completeSentence = do  -- F -> G
    putIOwordsT ["completeSentence F -> G ", showT resutl1F_readDocStringResult]
    s2 <- runErr $ mapM (completeOneDoc serverBrest German) (rightNote "isNotRight" resutl1F_readDocStringResult)
    assertEqual result1_G_readDocCompleted s2

rightNote :: Text ->  ErrOrVal a -> a
rightNote msg (Right a) = a
rightNote msg (Left t) = errorT [ msg, t]

completeOneDoc :: URI -> LanguageCode -> Doc0  -> ErrIO Doc0  -- F -> G
completeOneDoc serverloc lang doc = do
    let s1 = docSents doc
    s2 <- mapM (completeSentence False serverloc lang) s1
    return (doc {docSents = s2})



--right :: Either Text a -> a
--right (Left a) = errorT ["not a right",   a]
--right (Right a) = a

--test_1_E_F_readDocString = do   -- E -> F
--    putIOwords ["test_readDocString E -> F :  "] -- tripleResult]
--    let in1 :: [Text] = map (snd . right) (result1E ::[Either Text (NLPtext, Text)])
--    t1 <- runErr $ mapM (readDocString False) in1
--    putIOwords ["test_readDocString: result  ) ", showT  t1]
----    putIOwords ["test_parseToTZ:  result ", show' t1]
--    assertEqual resutl1F_readDocStringResult t1
--
--
--
#include "ProduceNLP.res"
-- result1X_CD, resutl1F_readDocStringResult, result1_G_readDocCompleted, nlpTriplesResult
