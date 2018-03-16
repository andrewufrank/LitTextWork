-----------------------------------------------------------------------------
--
-- Module      :  DocNLP_0or1
--
-- | the base data types for the Doc2 which results from reading the JSONflag
-- output from NLP

-- all data has 0 suffix
--when it appears the same both in XML and JSON

-- 1 if structure in JSON is different (Dependence, Coreference)
--and suffix 1 if it results from JSON conversion
-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
        , FlexibleInstances
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , RecordWildCards
    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CoreNLP.DocNLP_0or1 (
        module CoreNLP.DocNLP_0or1
        , module CoreNLP.ParseJsonCoreNLP
--        , module CoreNLP.POScodes
        ,  SpeakerTags (..)
--        , module CoreNLP.NERcodes -- import separately when needed
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
--        , module CoreNLP.DEPcodes  -- import separately when needed
        -- ,readDocString
        , unLCtext, LCtext (..), LanguageCodedText (..)
        )  where

import              Uniform.Strings
import Uniform.Zero
import   NLP.Corpora.Conll
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import CoreNLP.ParseJsonCoreNLP
import GHC.Generics
import qualified NLP.Types.Tags      as NLP
import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
import Data.Maybe
import LitTypes.LanguageTypedText (unLCtext, LCtext (..), LanguageCodedText (..) )
import qualified NLP.Corpora.Conll  as Conll -- for test

to1op :: Doc2  ->   (Doc1 Conll.POStag)  -- the entry point
to1op f =  convertTo1 Conll.undefConll English f


class ConvertTo1 postag a2 a1 where
    convertTo1 :: postag -> LanguageCode -> a2 -> a1




data Doc1 postag = Doc1 {doc1sents:: [Sentence1 postag]
                 , doc1corefs :: Maybe Coreferences1   -- only one
--                 , doc1id :: DocID     added in Doc11
                 } deriving (Show, Read, Eq, Ord, Generic)

instance Zeros (Doc1 postag) where zero = Doc1 [] zero

data Sentence1 postag = Sentence1 {s1id :: SentenceID
                        , s1parse :: Maybe Text  -- the parse tree
                        , s1toks :: [Token0 postag]
                        , s1deps :: Maybe [Dependence1]
                        -- only the last one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        }  deriving (Show, Read, Eq, Ord, Generic)


data Dependence1 = Dependence1 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID
                        , d1depid :: TokenID
                        , d1govGloss :: Text
                        , d1depGloss :: Text
                        } deriving (Show, Read, Eq, Ord, Generic)


instance Zeros Dependence1  where
        zero = Dependence1 zero zero zero zero zero zero

data Coreferences1 = Coreferences1 {coChains:: [MentionChain1]}
                deriving (Show, Read, Eq, Ord, Generic)

instance Zeros Coreferences1 where zero = Coreferences1 []

data MentionChain1 = MentionChain1 [Mention1]
        deriving (Show, Read, Eq, Ord, Generic)

instance Zeros (MentionChain1) where zero = MentionChain1 []

data Mention1 = Mention1 {mentRep ::  Bool -- , indicates the representative mention
        , mentSent :: SentenceID
        , mentStart, mentEnd :: TokenID -- not used ??
        , mentHead :: TokenID  -- the head of the mention
        , mentText :: Text  -- multiple words, the actual mention - not yet used
        -- coref_id not used
        , mentType :: Text
        , mentNumber :: Text
        , mentGender :: Text
        , mentAnimacy :: Text
        }
  deriving (Show, Read, Eq, Ord, Generic)
instance Zeros Mention1 where zero = Mention1 False zero zero zero zero zero
                                            zero zero zero zero

----instance Zeros Bool where zero = False

data Token0 postag = Token0 { tid :: TokenID
                    , tword :: Wordform0
                    , tlemma :: Lemma0
                    , tbegin, tend :: Int  -- not used
                    , tpos :: postag --  the pos tag recognized
                    , tposOrig :: Text -- the pos tag received
                    , tpostt :: Text -- the pos from the tree tagger
                    , tner :: [NERtag] -- [Text] -- String
                    , tspeaker :: [SpeakerTag] -- Text -- String
                    , tbefore, tafter :: Maybe Text
                    }   deriving (Show, Read, Eq, Ord, Generic)


--type DepTypeID0 = Text
--
--data DependenceType0 = DependenceType0 { dtt :: DepTypeID0
--                -- not used - whatever the last depType produced is taken
--            , dtd :: [Dependence0]
--            } deriving (Show, Read, Eq, Zeros)


data Dependence0 = Dependence0 {dtype :: DepCode -- Text -- String
                        , dorig :: Text -- the value given in the XML
                        , dgov :: DependencePart0
                        , ddep :: DependencePart0
                        } deriving   (Show, Read, Eq, Ord, Generic)

data DependencePart0 = DP0 { did :: TokenID  -- word number in sentence
                        , dword :: Wordform0  -- is word from text, not lemma
                            } deriving  (Show, Read, Eq, Ord, Generic)


instance (NLP.POStags postag) => ConvertTo1 postag Doc2 (Doc1 postag) where
--
--doc2to1 ::(NLP.POStags postag) => postag -> Doc2 -> Doc1 postag
--doc2to1
    convertTo1 posPh lang Doc2{..} = Doc1 {..}
      where
        doc1sents = map (convertTo1 posPh lang)  doc_sentences
        doc1corefs =  fmap (convertTo1 posPh lang)  doc_corefs
                -- chains of mentions
--        doc1id = zero


instance (NLP.POStags postag)
    => ConvertTo1 postag Sentence2 (Sentence1 postag) where

--sentence2to1 :: (NLP.POStags postag)
--    => postag -> Sentence2 -> Sentence1 postag

    convertTo1  posPh lang Sentence2 {..} = Sentence1 {..}
        where
            s1id = SentenceID s_index
            s1parse = s_parse
            s1toks = map (convertTo1 posPh lang)  s_tokens
            s1deps = case s_enhancedPlusPlusDependencies of
                Just d1 -> Just $ map (convertTo1  posPh lang) d1
                Nothing -> case s_enhancedDependencies of
                    Just d2 -> Just $ map  (convertTo1 posPh lang ) d2
                    Nothing -> case s_basicDependencies of
                        Just d3 -> Just $ map (convertTo1 posPh lang ) d3
                        Nothing -> Nothing


instance (NLP.POStags postag)
        => ConvertTo1 postag Token2 (Token0 postag) where

    convertTo1 posPh lang (Token2 {..}) = Token0 {..}

--token2to0 :: (NLP.POStags postag) => postag -> Token2 -> Token0 postag
---- ^ convert a token2 dataset from JSON to Token0
---- posTag phantom indicates the type of posTags to use
--token2to0 posPh (Token2 {..}) = Token0 {..}
      where
        tid = TokenID  tok_index
        tword = Wordform0 $ LCtext  tok_word lang
        tlemma = Lemma0 $ LCtext tok_lemma lang
        tpos = (NLP.parseTag  tok_pos) `asTypeOf` posPh
        tposOrig = tok_pos
        tpostt = zero
        tner = parseNERtagList [tok_ner] -- when is this a list?
                        -- use the Ner2 values?
        tspeaker = parseSpeakerTagList . maybeToList $ tok_speaker
--                    maybe [] (\a -> [a]) $ tok_speaker
        tbegin = tok_characterOffsetBegin
        tend = tok_characterOffsetEnd
        tbefore = tok_before
        tafter = tok_after


instance ConvertTo1 postag Dependency2 (Dependence1) where

--dependency2to0 :: Dependency2 -> Dependence1
    convertTo1 _ _ Dependency2 {..} = Dependence1 {..}
        where
            d1type = parseDEPtag dep_dep :: DepCode
            d1orig = dep_dep
            d1govid = TokenID dep_governor
            d1depid = TokenID dep_dependent
            d1govGloss = dep_governorGloss
            d1depGloss = dep_dependentGloss



instance ConvertTo1 postag Coreferences2 (Coreferences1) where

--coreferences2to0 :: Coreferences2 -> Coreferences1
    convertTo1 phP lang Coreferences2{..} = Coreferences1{..}
        where
            coChains = map (convertTo1 phP lang) chains

instance ConvertTo1 postag CorefChain2 MentionChain1 where

--corefChain2to0 :: CorefChain2 -> CorefChain2
    convertTo1 phP lang (CorefChain2 cs) = MentionChain1 (map (convertTo1 phP lang) cs)
        -- phantom is not used

instance ConvertTo1 postag Coref2 (Mention1) where

--coref2to0 :: Coref2 -> Mention1
    convertTo1 _ _ (Coref2 {..}) = Mention1 {..}
        where
            mentRep = coref_isRepresentativeMention
            mentSent = SentenceID coref_sentNum
            mentStart = TokenID coref_startIndex
            mentEnd = TokenID coref_endIndex   -- points next word
            mentHead = TokenID coref_headIndex
            mentText = coref_text
            mentType = coref_type
            mentNumber = coref_number
            mentGender = coref_gender
            mentAnimacy = coref_animacy

--
