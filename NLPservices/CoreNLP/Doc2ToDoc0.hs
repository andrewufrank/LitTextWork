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

module CoreNLP.Doc2ToDoc0 -- (openMain, htf_thisModuelsTests)
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
--parseNLP :: ErrIO ()

data Doc1 postag = Doc1 {doc1Sents:: [Sentence1 postag]
                 , doc1Corefs :: Coreferences1   -- only one
                       } deriving (Read, Show,  Eq)
instance Zeros (Doc1 postag) where zero = Doc1 [] zero

data Sentence1 postag = Sentence1 {s1id :: SentID0
                        , s1parse :: Text  -- the parse tree
                        , s1toks :: [Token0 postag]
                        , s1deps :: Maybe [Dependence1]
                        -- should be only one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        } deriving (Read, Show,  Eq)

data Dependence1 = Dependence1 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID0
                        , d1depid :: TokenID0
                        , d1govGloss :: Text
                        , d1depGloss :: Text
                        }   deriving (Show, Read, Eq)

instance Zeros Dependence1  where
        zero = Dependence1 zero zero zero zero zero zero
instance Zeros DepCode where zero = DepUnknown "constant zero"

data Coreferences1 = Coreferences1 {coChains:: [MentionChain1]}
                deriving (Read, Show,  Eq)

instance Zeros Coreferences1 where zero = Coreferences1 []

data MentionChain1 = MentionChain1 [Mention1] deriving (Read, Show,  Eq)

instance Zeros (MentionChain1) where zero = MentionChain1 []

data Mention1 = Mention1 {mentRep ::  Bool -- , indicates the representative mention
        , mentSent :: SentID0
        , mentStart, mentEnd :: TokenID0 -- not used ??
        , mentHead :: TokenID0  -- the head of the mention
        , mentText :: Text  -- multiple words, the actual mention - not yet used
        }
  deriving (Show, Read, Eq)
instance Zeros Mention1 where zero = Mention1 False zero zero zero zero zero


token2to0 :: (NLP.POStags postag) => postag -> Token2 -> Token0 postag
-- ^ convert a token2 dataset from JSON to Token0
-- posTag phantom indicates the type of posTags to use
token2to0 posPh (Token2 {..}) = Token0 {..}
    where
        tid = TokenID0  tok_index
        tword = Wordform0 tok_word
        tlemma = Lemma0 tok_lemma
        tpos = (NLP.parseTag  tok_pos) `asTypeOf` posPh
        tposOrig = tok_pos
        tpostt = zero
        tner = parseNERtagList [tok_ner] -- when is this a list?
                        -- use the Ner2 values?
        tspeaker = parseSpeakerTagList [tok_speaker]
        tbegin = tok_characterOffsetBegin
        tend = tok_characterOffsetEnd

coref2to0 :: Coref2 -> Mention1
coref2to0 (Coref2 {..}) = Mention1 {..}
    where
        mentRep = coref_isRepresentativeMention
        mentSent = SentID0 coref_sentNum
        mentStart = TokenID0 coref_startIndex
        mentEnd = TokenID0 coref_endIndex   -- points next word
        mentHead = TokenID0 coref_headIndex
        mentText = coref_text


dependency2to0 :: Dependency2 -> Dependence1
dependency2to0 Dependency2 {..} = Dependence1 {..}
    where
        d1type = parseDEPtag dep_dep :: DepCode
        d1orig = dep_dep
        d1govid = TokenID0 dep_governor
        d1depid = TokenID0 dep_dependent
        d1govGloss = dep_governorGloss
        d1depGloss = dep_dependentGloss


sentence2to1 :: (NLP.POStags postag)
    => postag -> Sentence2 -> Sentence1 postag

sentence2to1 posPh Sentence2 {..} = Sentence1 {..}
    where
            s1id = SentID0 s_index
            s1parse = s_parse
            s1toks = map (token2to0 posPh)  s_tokens
            s1deps = case s_enhancedPlusPlusDependencies of
                Just d1 -> Just $ map dependency2to0 d1
                Nothing -> case s_enhancedDependencies of
                    Just d2 -> Just $ map  dependency2to0 d2
                    Nothing -> case s_basicDependencies of
                        Just d3 -> Just $ map dependency2to0 d3
                        Nothing -> Nothing


coreferences2to0 :: Coreferences2 -> Coreferences1
coreferences2to0 Coreferences2{..} = Coreferences1{..}
    where
        coChains = map corefChain2to0 chains


corefChain2to0 :: CorefChain2 -> MentionChain1
corefChain2to0 (CorefChain2 cs) = MentionChain1 (map coref2to0 cs)

doc2to1 ::(NLP.POStags postag) => postag -> Doc2 -> Doc1 postag
doc2to1 posPh Doc2{..} = Doc1 {..}
    where
        doc1Sents = map (sentence2to1 posPh) doc_sentences
        doc1Corefs = coreferences2to0 doc_corefs
                -- chains of mentions

--
