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
    , module Parser.TextDescriptor
    , Triple, Snip, SnipID (..)
--    , TextDescriptor (..)
    ) where

--import           Test.Framework
--import Uniform.TestHarness
--import Parser.FormNLPsnips
--import Parser.FilterTextForNLP
import NLP2RDF.ProduceDocCallNLP
--import Parser.ProduceNLPtriples hiding ((</>))
--import Parser.CompleteSentence  (completeSentence, URI, serverBrest)
import          Data.RDF.FileTypes -- (ntFileTriples, ntFileTriplesGZip,writeHandleTriples)
import Data.Maybe (catMaybes)  -- todo
-- for tests:
import Parser.ReadMarkupAB
import LitTypes.TextDescriptor -- (TextDescriptor(..), serverLoc, originalsDir)
import NLP2RDF.ProduceDocCallNLP
import Uniform.FileIO (Path(..), Abs, File, TypedFiles5(..), Handle)
import Parser.FilterTextForNLP  (prepareTZ4nlp)
import Parser.FormNLPsnips (formSnips)
import LitTypes.LanguageTypedText -- (LanguageTypedText (..) )
import NLP2RDF.NLPvocabulary (SnipSigl (..))

-- debugNLP1 = False

-- main export

produceNLPtriples ::  TextDescriptor ->  [TZ2] -> ErrIO [Triple] -- test C  -> X
produceNLPtriples  textstate tzs =  do
    let     snips1 = prepareTZ4nlp posTag tzs :: [Snip]
            snips2 = formSnips snips1  :: [Snip]
            posTag = txPosTagset textstate
            debug = False
    let snips3 = zipWith pushSnipNumber2snip [1..] snips2
    triples :: [[Triple]] <- mapM (convertOneSnip2Triples debug  textstate)  snips3
--    ntz1 <- foldM writeHandleTriples (ntdescriptor textstate) triples
    return . concat $ triples
--    putIOwords ["\n\nproduceOneParaNLP nlp triples ", "one snip done"
--            ,"snip size", showT $ tz3textLength snip
--            ,"from text", buchName textstate
--            ]
--    return (textstate{ntdescriptor= ntz1})


pushPosTagset2snip :: TextDescriptor -> Snip -> Snip
pushPosTagset2snip textstate snip = snip {tz3posTag = txPosTagset textstate}

pushSnipNumber2snip :: Int -> Snip -> Snip
pushSnipNumber2snip i  snip = snip {tz3snipnr = i}


convertOneSnip2Triples :: Bool ->   TextDescriptor ->   Snip ->  ErrIO [Triple]
-- calls nlp to convert to doc
-- the snip should have a type parameter language
-- internal the text2nlp should have a tag type parameter
-- the triples (i.e. NLPtriples should have a tag parameter

-- the following is just the bridges, which should go earlier
convertOneSnip2Triples debugNLP textstate   snip = do
    let text = tz3text snip
    let language = getLanguageCode . tz3text $  snip    -- reduce for some special cases _italics_

    let nlpserver = nlpServer textstate
    let pt = txPosTagset textstate
    let (snipsigl, partOfTriples) = mkSnipPartOf textstate snip

    trips2 <- if not . notNullLC $ text
        then return zero
        else do
            trips <- convertOneSnip2Triples3 zero lang snip
            return trips

    return $ partOfTriples ++ trips2

--            case (language) of
--                (English, "") -> do
--                            t <- convertOneSnip2Triples2 undefEnglish
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                (German, "") -> do
--                            t <- convertOneSnip2Triples2 undefGerman undefGermanPos
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                (Italian,"") -> do
--                            t <- convertOneSnip2Triples2 undefItalian undefTinTPos
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                (French, "")-> do
--                            t <-convertOneSnip2Triples2 undefFrench undefFrenchPos
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                (French, "FrenchUD")-> do
--                            t <- convertOneSnip2Triples2 undefFrench undefFrenchUDPos
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                (Spanish,"") -> do
--                            t <- convertOneSnip2Triples2 undefSpanish undefSpanishPos
--                                        debugNLP   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                            return (map unNLPtriple t)
--                _ -> return zero
--            return trips

--        let paranum = tz3para snip
--    let parasigl = paraSigl textstate paranum
--    let snipsigl = mkSnipSigl parasigl snipnr
-- let buchURI = buchURIx   textstate
--    let paratrip = mkTriplePartOf (unSnipSigl snipsigl) (unParaSigl parasigl)
--    let buchtrip = mkTriplePartOf (unSnipSigl snipsigl) (buchURI)


mkSnipPartOf :: TextDescriptor -> Snip -> (SnipSigl, [Triple])
-- make the triples to state that triple is part of book and part of paragraphs2TZlayout
mkSnipPartOf textstate snip = (snipsigl, [buchtrip, paratrip])
    where
        paranum = tz3para snip
        parasigl = paraSigl textstate paranum
        snipsigl = mkSnipSigl parasigl (tz3snipnr snip) -- snipnr
--        nlpserver = nlpServer textstate
--        pt = txPosTagset textstate
        buchURI = buchURIx   textstate
        paratrip = mkTriplePartOf (unSnipSigl snipsigl) (unParaSigl parasigl)
        buchtrip = mkTriplePartOf (unSnipSigl snipsigl) (buchURI)





