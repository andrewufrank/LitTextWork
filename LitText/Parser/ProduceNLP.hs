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
import CoreNLP.Vocabulary
import Uniform.Zero
import NLP2RDF.ProduceDocCallNLP
import Parser.FilterTextForNLP  (prepareTZ4nlp)
import Parser.FormNLPsnips (formSnips)

produceNLPtriples :: LitTextFlags -> TextDescriptor -> [Snip] -> ErrIO [Triple]
            -- test C  -> X
produceNLPtriples  flags textstate snips3 =  do
    triples :: [[Triple]] <- mapM (convertOneSnip2Triples flags textstate) snips3
    return . concat $ triples

tz2toSnip :: LitTextFlags -> TextDescriptor ->  [TZ2] ->  [Snip]
-- convert the text to sinle language snips
tz2toSnip flags textstate tzs = snips3
   where
        posTag = txPosTagset textstate
        baserdf = buchURIx $ textstate
        snips1 = prepareTZ4nlp posTag baserdf tzs :: [Snip]
        snips2 = formSnips snips1  :: [Snip]
        snips3 = zipWith pushSnipNumber2snip [1..] snips2

--pushPosTagset2snip :: TextDescriptor -> Snip -> Snip
--pushPosTagset2snip textstate snip = snip {snip3posTag = txPosTagset textstate}

pushSnipNumber2snip :: Int -> Snip -> Snip
pushSnipNumber2snip i  snip = snip {snip3snipnr = SnipID i}

--pushSnipSigl2snip :: SnipSigl -> Snip -> Snip
--pushSnipSigl2snip s  snip = snip {tz3snipsigl = s}


convertOneSnip2Triples :: LitTextFlags -> TextDescriptor
            -> Snip ->  ErrIO [Triple]
-- calls nlp to convert to doc
-- the snip should have a type parameter language
-- internal the text2nlp should have a tag type parameter
-- the triples (i.e. NLPtriples should have a tag parameter

-- the following is just the bridges, which should go earlier
convertOneSnip2Triples flags textstate snip = do
    let text = snip3text snip
    let lang = getLanguageCode . snip3text $  snip
            -- reduce for some special cases _italics_

--    let nlpserver = nlpServer textstate
--    let pt = txPosTagset textstate
    let (snipsigl, partOfTriples) = mkSnipPartOf textstate snip

    trips2 <- if not . notNullLC $ text
        then return []
        else do
--            let snip2 = pushSnipSigl2snip snipsigl snip
            trips <- convertOneSnip2triples_NLPservices flags  snip
            -- call NLPservices from here
            return trips

    return $ partOfTriples ++ trips2


mkSnipPartOf :: TextDescriptor -> Snip -> (SnipSigl, [Triple])
-- make the triples to state that triple is part of
-- book and part of paragraphs2TZlayout
mkSnipPartOf textstate snip = (snipsigl, [buchtrip, paratrip])
    where
        paranum = zero -- ??? tz3para snip
        parasigl = paraSigl textstate paranum
        snipsigl = mkSnipSigl parasigl (snip3snipnr snip) -- snipnr
--        nlpserver = nlpServer textstate
--        pt = txPosTagset textstate
        buchURI = buchURIx   textstate
        paratrip = mkTriplePartOf (unSnipSigl snipsigl) (unParaSigl parasigl)
        buchtrip = mkTriplePartOf (unSnipSigl snipsigl) (buchURI)


--instance Zeros [Triple] where zero = []


