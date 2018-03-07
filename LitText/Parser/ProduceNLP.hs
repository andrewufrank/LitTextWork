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
--    , module Parser.TextDescriptor
    , Triple, Snip, SnipID (..), TZ2
    , TextDescriptor (..)
    ) where

import LitTypes.TextDescriptor
import NLP2RDF.NLPvocabulary
import Uniform.Zero
import NLP2RDF.ProduceDocCallNLP
import Parser.FilterTextForNLP  (prepareTZ4nlp)
import Parser.FormNLPsnips (formSnips)

produceNLPtriples ::  LitTextFlags -> TextDescriptor ->  [TZ2] -> ErrIO [Triple]
            -- test C  -> X
produceNLPtriples  flags textstate tzs =  do
    let     snips1 = prepareTZ4nlp posTag tzs :: [Snip]
            snips2 = formSnips snips1  :: [Snip]
            posTag = txPosTagset textstate
            debug = False
    let snips3 = zipWith pushSnipNumber2snip [1..] snips2
    triples :: [[Triple]] <- mapM (convertOneSnip2Triples flags  textstate)  snips3
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
pushSnipNumber2snip i  snip = snip {tz3snipnr = SnipID i}


convertOneSnip2Triples :: LitTextFlags ->   TextDescriptor ->   Snip ->  ErrIO [Triple]
-- calls nlp to convert to doc
-- the snip should have a type parameter language
-- internal the text2nlp should have a tag type parameter
-- the triples (i.e. NLPtriples should have a tag parameter

-- the following is just the bridges, which should go earlier
convertOneSnip2Triples flags textstate   snip = do
    let text = tz3text snip
    let lang = getLanguageCode . tz3text $  snip
            -- reduce for some special cases _italics_

    let nlpserver = nlpServer textstate
    let pt = txPosTagset textstate
    let (snipsigl, partOfTriples) = mkSnipPartOf textstate snip

    trips2 <- if not . notNullLC $ text
        then return []
        else do
            trips <- convertOneSnip2Triples3 flags lang snip snipsigl
            return trips

    return $ partOfTriples ++ trips2


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


--instance Zeros [Triple] where zero = []


