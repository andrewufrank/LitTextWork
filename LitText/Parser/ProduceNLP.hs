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
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLP
    (module Parser.ProduceNLP
--    , TextDescriptor (..)
    ) where

--import           Test.Framework
--import Uniform.TestHarness
--import Parser.FormNLPsnips
--import Parser.FilterTextForNLP
import Parser.ProduceDocCallNLP
import Parser.ProduceNLPtriples hiding ((</>))
import Parser.CompleteSentence  (completeSentence, URI, serverBrest)
import          Data.RDF.FileTypes (ntFileTriples, ntFileTriplesGZip)
import Data.Maybe (catMaybes)  -- todo
-- for tests:
import Parser.ReadMarkupAB
import Parser.TextDescriptor -- (TextDescriptor(..), serverLoc, originalsDir)
import Parser.ProduceDocCallNLP
import Uniform.FileIO (Path(..), Abs, File, TypedFiles5(..), resolveFile, Handle)
import Parser.FilterTextForNLP  (prepareTZ4nlp)
import Parser.FormNLPsnips (formSnips)
import Parser.LanguageTypedText -- (LanguageTypedText (..) )

-- debugNLP1 = False

-- main export

produceNLP ::  TextDescriptor ->  [TZ2] -> ErrIO TextDescriptor -- test C  -> X
produceNLP  textstate tzs =  do
    let     snips1 = prepareTZ4nlp posTag tzs :: [Snip]
            snips2 = formSnips snips1  :: [Snip]
            posTag = txPosTagset textstate
            debug = False
    triples :: [[Triple]] <-zipWithM (convertOneSnip2Triples debug  textstate) (map SnipID [1..]) snips2
    ntz1 <- foldM writeHandleTriples textstate triples
--    putIOwords ["\n\nproduceOneParaNLP nlp triples ", "one snip done"
--            ,"snip size", showT $ tz3textLength snip
--            ,"from text", buchName textstate
--            ]
    return ntz1


pushPosTagset2snip :: TextDescriptor -> Snip -> Snip
pushPosTagset2snip textstate snip = snip {tz3posTag = txPosTagset textstate}



convertOneSnip2Triples :: Bool ->   TextDescriptor -> SnipID -> Snip ->  ErrIO [Triple]
-- calls nlp to convert to doc
-- the snip should have a type parameter language
-- internal the text2nlp should have a tag type parameter
-- the triples (i.e. NLPtriples should have a tag parameter

-- the following is just the bridges, which should go earlier
convertOneSnip2Triples debugNLP textstate snipnr snip = do
    let text = tz3text snip
    let language = getLanguageCode . tz3text $  snip    -- reduce for some special cases _italics_
--    let buchname = buchName textstate
    let paranum = tz3para snip
    let parasigl = paraSigl textstate paranum
    let snipsigl = mkSnipSigl parasigl snipnr
    let nlpserver = nlpServer textstate
    let pt = txPosTagset textstate
    trips2 <- if not . notNullLC $ text
        then return zero
        else do
            trips <- case (language, pt) of
                (English, "") -> do
                            t <- convertOneSnip2Triples2 undefEnglish undefConll
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                (German, "") -> do
                            t <- convertOneSnip2Triples2 undefGerman undefGermanPos
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                (Italian,"") -> do
                            t <- convertOneSnip2Triples2 undefItalian undefTinTPos
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                (French, "")-> do
                            t <-convertOneSnip2Triples2 undefFrench undefFrenchPos
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                (French, "FrenchUD")-> do
                            t <- convertOneSnip2Triples2 undefFrench undefFrenchUDPos
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                (Spanish,"") -> do
                            t <- convertOneSnip2Triples2 undefSpanish undefSpanishPos
                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                            return (map unNLPtriple t)
                _ -> return zero
            return trips
    let buchURI = buchURIx   textstate
    let paratrip = mkTriplePartOf (unSnipSigl snipsigl) (unParaSigl parasigl)
    let buchtrip = mkTriplePartOf (unSnipSigl snipsigl) (buchURI)
    return $ buchtrip : paratrip : trips2



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
                putIOwords ["openHandleTriples - error ", e , "file", showT $ destNT textstate]
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





