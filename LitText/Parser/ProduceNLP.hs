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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLP
    (module Parser.ProduceNLP
    , Triple, Snip, SnipID (..), TZ2
    , TextDescriptor (..)
    ) where

import CoreNLP.Vocabulary
import Uniform.Zero
import NLP2RDF.ProduceDocCallNLP
import Parser.FilterTextForNLP  (prepareTZ4nlp)
import Parser.FormNLPsnips (formSnips)

produceNLPtriples :: LitTextFlagSet -> TextDescriptor -> [Snip] -> ErrIO [Triple]
            -- test C  -> X
produceNLPtriples  flags textstate snips3 =  do
    triples :: [[Triple]] <- mapM
                (convertOneSnip2Triples flags textstate) snips3
    return . concat $ triples

tz2toSnip ::  TextDescriptor ->  [TZ2] ->  [Snip]
-- convert the text to sinle language snips
tz2toSnip textstate tzs = snips3
   where
        posTag = txPosTagset textstate
        baserdfx = buchURIx $ textstate
        snips1 = prepareTZ4nlp posTag baserdfx tzs :: [Snip]
        snips2 = formSnips snips1  :: [Snip]
        snips3 = zipWith pushSnipNumber2snip (map SnipID [1..]) snips2
        -- set snipnumber at end

--        pushSnipNumber2snip :: Int -> Snip -> Snip
--        pushSnipNumber2snip i  snip = snip {snip3snipnr = SnipID i}



convertOneSnip2Triples ::   LitTextFlagSet -> TextDescriptor
            -> Snip ->  ErrIO [Triple]
-- calls nlp to convert to doc
-- the snip should have a type parameter language
-- internal the text2nlp should have a tag type parameter
-- the triples (i.e. NLPtriples should have a tag parameter

-- the following is just the bridges, which should go earlier
convertOneSnip2Triples flags textstate snip = do
    let text = snip3text snip
    let (_, partOfTriples) = mkSnipPartOf textstate snip
    trips2 <- if not . notNullLC $ text
        then return []
        else do
            let snip2 = convertOneSnip2TriplesX flags textstate snip
            trips <- convertOneSnip2triples_NLPservices flags  snip2
            -- call NLPservices from here
            return trips
    putIOwords ["convertOneSnip2Triples", "a snip of length "
                    , showT (snip3textLength snip), "char converted"]

    return $ partOfTriples ++ trips2

convertOneSnip2TriplesX flags textstate snip = do
    let text = snip3text snip
    let lang = getLanguageCode . snip3text $  snip
            -- reduce for some special cases _italics_

--    let nlpserver = nlpServer textstate
--    let pt = txPosTagset textstate
    let (snipsigl, _) = mkSnipPartOf textstate snip

    pushSnipSigl2snip (unSnipSigl snipsigl) snip


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




