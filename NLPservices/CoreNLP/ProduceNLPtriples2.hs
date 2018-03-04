-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- all data additional to Defs0 have 1 suffix
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

--{-# LANGUAGE TemplateHaskell #-}
-- template haskell requires reordering of data types
--and all functions used for default otions imported (not local defined)

module CoreNLP.ProduceNLPtriples2 -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
--import Uniform.FileIO
import CoreNLP.Defs0
import CoreNLP.ParseJsonCoreNLP
import qualified NLP.Types.Tags      as NLP
--import qualified NLP.Corpora.Conll  as Conll
--import qualified NLP.Corpora.UD  as UD
--            Uniform.FileIO
--import              LitNLP.Tools
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import Uniform.Zero
import Parser.ProduceNLPtriples
import CoreNLP.Defs0
import CoreNLP.NERcodes
import Parser.TextDescriptor
import NLP.Types.Tags
import Parser.NLPvocabulary  -- from Foundation
import Parser.LanguageTypedText
import Data.List (partition)
import Data.RDF.Types  (Triple)  -- instance Show Triple
import CoreNLP.Doc2ToDoc0

processDoc1toTriples2 :: (Show postag, POStags postag, LanguageTypedText lang)
            => lang ->  postag -> SnipSigl  -> Doc1 postag -> [NLPtriple postag]

processDoc1toTriples2  lph pph snipsigl  Doc1{..} =
    map NLPtriple $ t2  : sents  ++ corefs

    where
        lang = languageCode lph -- tz3lang ntz
--        snipid = snip2sigl snip -- mkSnipSigl paraid snipnr
        t2 = mkTripleText (unSnipSigl snipsigl) (mkRDFproperty LanguageTag) (showT lang)
        sents :: [Triple]
        sents =   concat $ map (mkSentence1Triple2 lang  snipsigl) (doc1Sents)
        corefs =   concat $ map  (mkCorefTriple2 lang   snipsigl )
                            (coChains doc1Corefs )

----------------------
mkSentence1Triple2 :: (Show postag, POStags postag) =>
        LanguageCode ->  SnipSigl  ->    Sentence1 postag ->  ( [Triple])
-- ^ produce the   triples for a sentence
mkSentence1Triple2 lang snipid Sentence1 {..}
       =      t0 : t1 : t2 :  sentenceForm :  (toktrips ++ depsTrips)
    where
        sentSigl = mkSentSigl snipid s1id
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse)
                                 s1parse
        t2 = mkTriplePartOf (unSentSigl sentSigl) --`buchuri
                             (unSnipSigl snipid)
        depsTrips =  maybe [] ( (mkDependence1Triples2 lang sentSigl) )
                                  s1deps   :: [Triple]
                                -- in CoreNLPxml only the last dependency analysis, if any, is retained
        toktrips =  concatMap (mkTokenTriple2 lang sentSigl)
                                s1toks :: [Triple]
        sentenceForm = mkTripleLang3 lang (unSentSigl sentSigl) (mkRDFproperty SentenceForm)
                    (unwords' . map (word0 . tword) $ s1toks   )

mkDependence1Triples2 :: LanguageCode -> SentSigl ->    [Dependence1] -> [Triple]
mkDependence1Triples2 lang sentSigl  ds   =
--              t1 : t2 :
                     ts
    where
        ts = concat $ zipWith (mkDependence1Triple2 lang sentSigl  ) ds [1..]
            -- the id for deps should not be needde
                -- passes sentSigl to construct the tokenid later

mkDependence1Triple2 :: LanguageCode -> SentSigl ->  Dependence1 -> Int -> [Triple]
mkDependence1Triple2 lang sentid  Dependence1{..} i  =
    [mkTripleRef (unTokenSigl govtokenid)
                                             (mkRDFproperty d1type)
                                             (unTokenSigl deptokenid)]
-- dependence construction produces incorrect (white space, " etc in depSigl
    where
--        dt = dtype  -- the depence type
--
--        govTok =    d1govid     -- tokenID in sentence
--        depTok =   d1depid
        govtokenid = mkTokenSigl sentid d1govid
        deptokenid = mkTokenSigl sentid d1depid
        -- code to preserve structure of xml
        -- =  t0:  t4 : (t5 ++ t6 ++ t7)

-------------- coreferences ---------------------
--mkCorefTriple1 :: LanguageCode -> SnipSigl -> MentionChain0  ->   [Triple]
---- ^ gives a single set of coreferences  - int is to produce id -- not required
---- ^ produces triples from the mention to the representative one
--
--mkCorefTriple1 lang snipsigl coref    =
--        if null . corefCluster $ coref
--            then []
--            else concat $ map (mkCorefTriple2 lang snipsigl) (corefCluster coref)


mkCorefTriple2 :: LanguageCode -> SnipSigl -> MentionChain1  ->  [Triple]
-- ^ gives a single set of coreferences  - int is to produce id -- not required
-- ^ produces triples from the mention to the representative one

mkCorefTriple2 lang snipsigl (MentionChain1 mentions)   = if null mentions then []
                                        else map (mkRefs repSigl) mentSigl
    where

--            mentions = corefMents ments

            (rep,norep) = partition mentRep mentions

            rep' = case length rep of
                1 -> headNote "mkCorefTriple2 rep not present" rep :: Mention1
                _ -> errorT ["mkCoreTriple2 - mentions exist, but not a single true rep",
                        showT mentions, showT rep]
            heads = map mentHead norep
                    -- the heads are the references

            repSigl = mkRefSigl snipsigl rep' :: TokenSigl
            mentSigl = map (mkRefSigl snipsigl) norep  :: [TokenSigl]


mkRefSigl :: SnipSigl -> Mention1 -> TokenSigl
mkRefSigl s m = mkTokenSigl  sentid . mentHead $ m
    where       sentid = mkSentSigl  s  (mentSent m)

mkRefs :: TokenSigl -> TokenSigl -> Triple
mkRefs h m = mkTripleRef (unTokenSigl m) (mkRDFproperty Mentions) (unTokenSigl h)
        -- a is a mention of h


--
