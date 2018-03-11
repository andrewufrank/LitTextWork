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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

--{-# LANGUAGE TemplateHaskell #-}
-- template haskell requires reordering of data types
--and all functions used for default otions imported (not local defined)

module CoreNLP.Doc2ToRDF_JSON
    ( module CoreNLP.Doc2ToRDF_JSON
    ,  module CoreNLP.DocNLP_0or1
    ,
    ) where

import           Uniform.Strings
import CoreNLP.DocNLP_0or1
import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
import qualified NLP.Types.Tags      as NLP
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import Uniform.Zero
import Data.Maybe

--instance (NLP.POStags postag) => To1 postag Doc2 (Doc1 postag) where
----
----doc2to1 ::(NLP.POStags postag) => postag -> Doc2 -> Doc1 postag
----doc2to1
--    to1 posPh Doc2{..} = Doc1 {..}
--      where
--        doc1Sents = map (to1 posPh) doc_sentences
--        doc1Corefs =  fmap (to1 posPh) doc_corefs
--                -- chains of mentions
--
--
--instance (NLP.POStags postag)
--    => To1 postag Sentence2 (Sentence1 postag) where
--
----sentence2to1 :: (NLP.POStags postag)
----    => postag -> Sentence2 -> Sentence1 postag
--
--    to1  posPh Sentence2 {..} = Sentence1 {..}
--        where
--            s1id = SentenceID s_index
--            s1parse = s_parse
--            s1toks = map (to1 posPh)  s_tokens
--            s1deps = case s_enhancedPlusPlusDependencies of
--                Just d1 -> Just $ map (to1 posPh) d1
--                Nothing -> case s_enhancedDependencies of
--                    Just d2 -> Just $ map  (to1 posPh) d2
--                    Nothing -> case s_basicDependencies of
--                        Just d3 -> Just $ map (to1 posPh) d3
--                        Nothing -> Nothing
--
--
--instance (NLP.POStags postag)
--        => To1 postag Token2 (Token0 postag) where
--
--    to1 posPh (Token2 {..}) = Token0 {..}
--
----token2to0 :: (NLP.POStags postag) => postag -> Token2 -> Token0 postag
------ ^ convert a token2 dataset from JSON to Token0
------ posTag phantom indicates the type of posTags to use
----token2to0 posPh (Token2 {..}) = Token0 {..}
--      where
--        tid = TokenID  tok_index
--        tword = Wordform0 tok_word
--        tlemma = Lemma0 tok_lemma
--        tpos = (NLP.parseTag  tok_pos) `asTypeOf` posPh
--        tposOrig = tok_pos
--        tpostt = zero
--        tner = parseNERtagList [tok_ner] -- when is this a list?
--                        -- use the Ner2 values?
--        tspeaker = parseSpeakerTagList . maybeToList $ tok_speaker
----                    maybe [] (\a -> [a]) $ tok_speaker
--        tbegin = tok_characterOffsetBegin
--        tend = tok_characterOffsetEnd
--
--
--instance To1 postag Dependency2 (Dependence1) where
--
----dependency2to0 :: Dependency2 -> Dependence1
--    to1 _  Dependency2 {..} = Dependence1 {..}
--        where
--            d1type = parseDEPtag dep_dep :: DepCode
--            d1orig = dep_dep
--            d1govid = TokenID dep_governor
--            d1depid = TokenID dep_dependent
--            d1govGloss = dep_governorGloss
--            d1depGloss = dep_dependentGloss
--
--
--
--instance To1 postag Coreferences2 (Coreferences1) where
--
----coreferences2to0 :: Coreferences2 -> Coreferences1
--    to1 phP Coreferences2{..} = Coreferences1{..}
--        where
--            coChains = map (to1 phP) chains
--
--instance To1 postag CorefChain2 MentionChain1 where
--
----corefChain2to0 :: CorefChain2 -> CorefChain2
--    to1 phP (CorefChain2 cs) = MentionChain1 (map (to1 phP) cs)
--        -- phantom is not used
--
--instance To1 postag Coref2 (Mention1) where
--
----coref2to0 :: Coref2 -> Mention1
--    to1 _  (Coref2 {..}) = Mention1 {..}
--        where
--            mentRep = coref_isRepresentativeMention
--            mentSent = SentenceID coref_sentNum
--            mentStart = TokenID coref_startIndex
--            mentEnd = TokenID coref_endIndex   -- points next word
--            mentHead = TokenID coref_headIndex
--            mentText = coref_text
--
----
