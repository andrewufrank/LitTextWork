-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | consist of two parts - preparation of the data to submit and
-- analysis of the return
--
-- version 2 assumes that each paragraph is individually analyzed
--  for german - the lemma are determined for each sentence individually
-- using the tokenization from the coreNLP

-- later:
-- snippets are pieces in one paragraph of one languageBreakCode
-- therefore the snippet id is paragraph id + count
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

module Parser.ProduceNLPtriples
    (produceNLPtriples
    , TextState2 (..)
    , htf_thisModulesTests
    , result1E_nlpResult
    , TZ (..)
--    , storeTriplesFuseki
    , writeTriples2file
    ) where

import           Test.Framework

import Parser.Foundation  hiding ((</>)) -- for TZ
import           Store.Fuseki
import Parser.ProduceNLP
import Uniform.Error   -- For ErrOrVal

import           CoreNLP.Defs0
import           CoreNLP.Snippets2nt          (readDocString)
import           Data.RDF   -- should all be imported from extension
import           Data.RDF.Extension
import          Data.RDF.FileTypes
import Lines2para.Lines2para
--import           Parser.LinesToParagraphs (result1A)  -- for test
--import Lines2para.Lines2paraTests (result1A)
import           Parser.NLPvocabulary
--import Parser.ProduceLit (result1C)   -- for test
import           Uniform.Strings              hiding ((<|>))
import Uniform.FileIO
import Parser.CompleteSentence -- (completeSentence)
-- for tests
import Parser.ReadMarkupAB -- (result1A)
--import Lines2para.Lines2paraTests  -- (result1C)
import Data.Either

debugNLP1 = False

-- main export
produceNLPtriples :: TextState2 -> [TZ3] -> ErrIO () -- test C -> D -> X
-- produce the triples and store them in triple store,
-- first extract only the text TZ lines and convert the hyphenated texts
-- repeated for each paragraph
produceNLPtriples textstate = mapM_ (produceOneParaNLP debugNLP1 textstate) . prepareTZ4nlp

        -- prepareTZ4nlp is in ProduceNLP

test_1_D_XproduceNLPtriples =  do   -- test D -> H
    putIOwords ["produceNLPtriples:  D -> H  "] -- , showT tzResult]
    t1 <-   runErr $ produceNLPtriples  result1A result1C
--    putIOwords ["produceNLPtriples: result (for next) ", s2t $ show t1]
--    putIOwords ["produceNLPtriples:  result ", showT t1]
    assertEqual (Right ())  t1

produceOneParaNLP :: Bool -> TextState2 -> TZ3 -> ErrIO ()
produceOneParaNLP showXML textstate tzp =
        do
            (tz, xml) <- convertTZ2nlp (serverLoc textstate) tzp  -- calls to coreNLP    D -> E
            -- tests: result1E_nlpResult
--            tri1 <- produceNLPtriples2 showXML textstate tz_text
            doc0 <- readDocString showXML xml                    -- E -> F
            --  tests: readDocStringResult

            let lang = tzlang tzp
            let sents1 = docSents doc0

            sents2 <- if lang == German
                    then mapM (completeSentence False lang) sents1  -- F -> G
                    else return sents1

            let doc0' = doc0{docSents = sents2}

            when debugNLP $
                    putIOwords ["\nproduceOneParaNLP read doc0", showT doc0', "\n"]
        --    let buchuri = buchURIx textstate :: RDFsubj
            let triples  = processDoc0toTriples2 textstate tz doc0'  -- G -> H

            when debugNLP $
                putIOwords ["\n\nproduceOneParaNLP nlp triples ", unlines' . map showT $ triples]
            writeTriples2file textstate triples
--            when debugNLP $ putIOwords ["produceOneParaNLP triples stored   "
--                        , showT . textfilename $ textstate, " \n", showT response ]
--            let response2 = response <>
--                        (showT . tlpara . tzloc $ tzp) <> "on page" <>
--                        (showT . tlpage . tzloc $ tzp)
            return ()

writeTriples2file :: TextState2 -> [Triple] -> ErrIO ()
writeTriples2file textstate tris = do
    write6 (textfilename textstate)  ntFileTriples tris
    -- putIOwords ["storeTriplesFuseki", "endpoint", endpoint textstate]
--    insertTriplesIntoGraph fusekiServer (endpoint textstate)
--            tris  (Just (gerastreeURI </> graph textstate ))

--storeTriplesFuseki textstate tris = do
--    -- putIOwords ["storeTriplesFuseki", "endpoint", endpoint textstate]
--    insertTriplesIntoGraph fusekiServer (endpoint textstate)
--            tris  (Just (gerastreeURI </> graph textstate ))

test_completeSentence = do  -- F -> G
    putIOwordsT ["completeSentence F -> G ", showT resutl1F_readDocStringResult]
    s2 <- runErr $ mapM (completeOneDoc German) (rightNote "isNotRight" resutl1F_readDocStringResult)
    assertEqual result1_G_readDocCompleted s2

rightNote :: Text ->  ErrOrVal a -> a
rightNote msg (Right a) = a
rightNote msg (Left t) = errorT [ msg, t]

completeOneDoc :: LanguageCode -> Doc0  -> ErrIO Doc0  -- F -> G
completeOneDoc lang doc = do
    let s1 = docSents doc
    s2 <- mapM (completeSentence False lang) s1
    return (doc {docSents = s2})

processDoc0toTriples2 :: TextState2 -> TZ3 ->  Doc0 -> [Triple] -- TriplesGraph  G -> H
-- ^ convert the doc0 (which is the analysed xml) and produce the triples
processDoc0toTriples2 textstate tz  doc0  =       t2  :  concat [sents]
                    -- , corefs] corefs not produced
    where
        lang = tzlang tz
        paraid = paraSigl textstate tz
        snipid = paraid
        t2 = mkTripleText (unParaSigl snipid) (mkRDFproperty LanguageTag) (showT lang)
        sents :: [Triple]
        sents = concat $ map (mkSentenceTriple2 lang  snipid) (docSents doc0)
 --        corefs = concat $ map (mkCorefTriple2 lang   snipid ) (docCorefs doc0)
-- currently not producing the not yet used corefs


----------------------
mkSentenceTriple2 :: LanguageCode ->   DocSigl  ->    Sentence0 ->  ( [Triple])
-- ^ produce the   triples for a sentence
-- filters out empty (. only) sentences
-- and recognized the count sentences
mkSentenceTriple2 lang   snipid sent
--       | isEmptySentence =(paragraphcount0,[])
--       | isCountSentence =   (paragraphcount, []) -- [paragraphcountTriple])
       -- only for count sentence change paragraph count
--       | otherwise
       =  t0 : t1 : t2 : senteceForm : (toktrips)   --  ++ depsTrips))
       -- production of dependenceTypes removed - not yet used
    where

        sentSigl = mkSentSigl snipid (sid sent)
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse) (sparse sent)
        t2 = mkTriplePartOf (unSentSigl sentSigl) (unDocSigl snipid)
        -- depsTrips = maybe []  (mkDependenceTypeTriples2 lang sentSigl ) ( sdeps sent) :: [Triple]
        toktrips = concat . map (mkTokenTriple2 lang sentSigl) $ (stoks sent):: [Triple]
--        toktrips = (mkTokenTriple2 lang sentSigl) $ (concat . stoks $ sent):: [Triple]
        senteceForm = mkTripleLang lang (unSentSigl sentSigl) (mkRDFproperty SentenceForm)
                    (unwords' . map (word0 . tword) . stoks $ sent )

----------------------------------------- --
mkTokenTriple2 :: LanguageCode -> SentSigl-> Token0 -> [Triple]
mkTokenTriple2 lang sentSigl tok =  [t0, t1, t2, t2a, t3, t5] ++ t6 ++ t7
    -- the language code is to pass to the triple maker ! TODO
    where
        tokensigl = mkTokenSigl sentSigl (tid tok)
        t0 = mkTripleType (unTokenSigl tokensigl) (mkRDFtype Token)
        t1 = mkTriplePartOf (unTokenSigl tokensigl)     (unSentSigl sentSigl)
        t2 = mkTripleLang lang (unTokenSigl tokensigl) (mkRDFproperty Lemma) (decode lang . lemma0 $ tlemma tok)
        t2a = mkTripleLang3 lang (unTokenSigl tokensigl) (mkRDFproperty Lemma3) (decode lang . lemma0 $ tlemma tok)
        t3 = mkTripleText (unTokenSigl tokensigl) (mkRDFproperty Pos) (show' . tpos $ tok)  -- use the encoding from Conll
        -- t4 = mkTripleLang lang tokenid lemmapPoSProp $ mkLemmaPoS (tlemma tok) (tpos tok)
--            ((lemma0 .t1lemma $ tok) <:> (show' . t1pos $tok))
        t5 = mkTripleLang lang (unTokenSigl tokensigl) (mkRDFproperty WordForm) (decode lang . word0 . tword $ tok)
        t6 = map (mkTripleText (unTokenSigl tokensigl) (mkRDFproperty Nertag)) (tner tok)
        t7 = map (mkTripleText (unTokenSigl tokensigl) (mkRDFproperty SpeakerTag) . showT )
                    (tspeaker tok)


mkLemmaPoS :: Lemma0 -> Pos -> Text
-- to avoid including punctuation characters in the lemmaPoS
-- TODO check
mkLemmaPoS l p = if isPOSpunctuation p then show' p
                    else  (lemma0 l) <:> (show' p)

----------------------------------



right :: Either Text a -> a
right (Left a) = errorT ["not a right",   a]
right (Right a) = a

test_1_E_F_readDocString = do   -- E -> F
    putIOwords ["test_readDocString E -> F :  "] -- tripleResult]
    let in1 :: [Text] = map (snd . right) (result1E ::[Either Text (TZ3, Text)])
    t1 <- runErr $ mapM (readDocString False) in1
    putIOwords ["test_readDocString: result  ) ", showT  t1]
--    putIOwords ["test_parseToTZ:  result ", show' t1]
    assertEqual resutl1F_readDocStringResult t1


--
decode :: LanguageCode -> Text -> Text
decode lang wf = wf
--                case lang of
--                        English -> wf
--                        German ->   wf -- s2t . latin1ToUnicode . t2s $ wf
----                        German ->   decodeLatin1 . t2b $ wf
----                        German -> fromJustNote "decode from german" . b2t . convert "Latin1" "UTF-8"  . t2b $ wf


#include "ProduceNLP.res"
-- result1X_CD, resutl1F_readDocStringResult, result1_G_readDocCompleted, nlpTriplesResult
