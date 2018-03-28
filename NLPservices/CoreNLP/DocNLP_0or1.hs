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

-- converts the .doc2 files to .doc3
-- put the text in LCtext with language code
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
import   NLP.TagSets.Conll hiding (NERtag (..))
import              NLP.TagSets.DEPcodes
import              NLP.TagSets.NERcodes
import              NLP.TagSets.SpeakerTags
import CoreNLP.ParseJsonCoreNLP
import GHC.Generics
import qualified NLP.Tags      as NLP
import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
import Data.Maybe
import LitTypes.LanguageTypedText (unLCtext, LCtext (..), LanguageCodedText (..) )
import qualified NLP.TagSets.Conll  as Conll -- for test

to1op :: (POStags postag) =>
        postag -> LanguageCode -> Doc2  ->   (Doc1 postag)
-- the entry point
to1op postag lang f =  convertTo1 postag lang f


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
                        , s1entitymentions :: Maybe [Ner3]
                        }  deriving (Show, Read, Eq, Ord, Generic)


data Dependence1 = Dependence1 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID
                        , d1depid :: TokenID
                        , d1govGloss :: LCtext
                        , d1depGloss :: LCtext
                        } deriving (Show, Read, Eq, Ord, Generic)


instance Zeros Dependence1  where
        zero = Dependence1 unkDEPtag zero zero zero zero zero

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
        , mentText :: LCtext   -- multiple words, the actual mention - not yet used
        -- coref_id not used
        , mentType :: Text
        , mentNumber :: Text
        , mentGender :: Text
        , mentAnimacy :: Text
        }
  deriving (Show, Read, Eq, Ord, Generic)
instance Zeros Mention1 where zero = Mention1 False zero zero zero zero zero
                                            zero zero zero zero
-- | the record from the s_entitymentions
data Ner3 = Ner3 {ner3docTokenBegin :: TokenID
                , ner3docTokenEnd :: TokenID
                , ner3tokenBegin :: TokenID
                , ner3tokenEnd :: TokenID
                , ner3text :: LCtext
                , ner3characterOffsetBegin :: Int
                , ner3characterOffsetEnd :: Int
                , ner3ner :: NERtag -- the code ??
                }
        deriving (Show, Read, Eq, Ord, Generic)

----instance Zeros Bool where zero = False

data NERtagExt = NERtagValue Text | NER NERtag
  deriving (Show, Read, Eq, Ord, Generic)

data Token0 postag = Token0 { tid :: TokenID
                    , tword :: Wordform0
                    , twordOrig :: Maybe Text
                    , tlemma :: Lemma0
                    , tbegin, tend :: Int  -- not used
                    , tpos :: postag --  the pos tag recognized
                    , tfeature :: [(String,String)] -- the features, improve rep!!!
                            -- only for UD, could be included in UD pos tag ?

                    , tposOrig :: Maybe Text -- the pos tag received
--                    , tpostt :: Maybe Text -- the pos from the tree tagger
                    , tner :: [NERtagExt] -- [Text] -- String
                    , tnerOrig :: Maybe [Text]
                    , tspeaker :: [SpeakerTag] -- Text -- String
                    , tbefore, tafter :: Maybe Text
                    }   deriving (Show, Read, Eq, Ord, Generic)


--type DepTypeID0 = Text
--
--data DependenceType0 = DependenceType0 { dtt :: DepTypeID0
--                -- not used - whatever the last depType produced is taken
--            , dtd :: [Dependence0]
--            } deriving (Show, Read, Eq, Zeros)


-- old - were used for xml corenlp output

data Dependence0 = Dependence0 {dtype :: DepCode -- Text -- String
                        , dorig :: Text -- the value given in the XML
                        , dgov :: DependencePart0
                        , ddep :: DependencePart0
                        } deriving   (Show, Read, Eq, Ord, Generic)

data DependencePart0 = DP0 { did :: TokenID  -- word number in sentence
                        , dword :: Wordform0  -- is word from text, not lemma
                            } deriving  (Show, Read, Eq, Ord, Generic)

-----------------------

--                c o n v e r s i o n s


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
            s1id = SentenceID (s_index + 1)
                -- the index in json starts with 0, but sentence numbers start with 1
            s1parse = s_parse
            s1toks = map (convertTo1 posPh lang)  s_tokens
            s1deps = case s_enhancedPlusPlusDependencies of
                Just d1 -> Just $ map (convertTo1  posPh lang) d1
                Nothing -> case s_enhancedDependencies of
                    Just d2 -> Just $ map  (convertTo1 posPh lang ) d2
                    Nothing -> case s_basicDependencies of
                        Just d3 -> Just $ map (convertTo1 posPh lang ) d3
                        Nothing -> Nothing
            s1entitymentions = fmap (map (convertTo1 posPh lang)) s_entitymentions


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
        twordOrig = if tok_word == tok_originalText then Nothing else Just tok_originalText
        tlemma = Lemma0 $ LCtext tok_lemma lang
        tpos = (NLP.toPOStag  tok_pos) `asTypeOf` posPh
        tfeature = []
        tposOrig = if showT tpos == tok_pos then Nothing else Just tok_pos
        -- missig a test that parse was complete
--        tpostt = Nothing
        tner = parseNERtagList [tok_ner] -- when is this a list?
        tnerOrig = if (any (==(NER NERunk)) $ tner) then Just [tok_ner] else Nothing
                        -- use the Ner2 values?
        tspeaker = parseSpeakerTagList . maybeToList $ tok_speaker
--                    maybe [] (\a -> [a]) $ tok_speaker
        tbegin = tok_characterOffsetBegin
        tend = tok_characterOffsetEnd
        tbefore = tok_before
        tafter = tok_after


parseNERtagList :: [Text] -> [NERtagExt]
parseNERtagList [] = []
parseNERtagList [a] = [NER $ toNERtag a]
parseNERtagList (a:as) = (NER $ toNERtag a) : map NERtagValue as
    -- assume the first is the code and the rest is detail
    -- check with actual values

--isAnUnknownNER  (NERunk a) = True
--isAnUnknownNER  _ = False

instance ConvertTo1 postag Dependency2 (Dependence1) where

--dependency2to0 :: Dependency2 -> Dependence1
    convertTo1 _ lang Dependency2 {..} = Dependence1 {..}
        where
            d1type = toDEPtag dep_dep :: DepCode
            d1orig = dep_dep
            d1govid = TokenID dep_governor
            d1depid = TokenID dep_dependent
            d1govGloss = LCtext {ltxt = dep_governorGloss, llang = lang}
            d1depGloss = LCtext {ltxt = dep_dependentGloss, llang = lang}



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
    convertTo1 _ lang  (Coref2 {..}) = Mention1 {..}
        where
            mentRep = coref_isRepresentativeMention
            mentSent = SentenceID coref_sentNum
            mentStart = TokenID coref_startIndex
            mentEnd = TokenID coref_endIndex   -- points next word
            mentHead = TokenID coref_headIndex
            mentText = LCtext {ltxt = coref_text, llang = lang}
            mentType = coref_type
            mentNumber = coref_number
            mentGender = coref_gender
            mentAnimacy = coref_animacy


instance ConvertTo1 postag Ner2 Ner3 where
    convertTo1 _ lang Ner2{..} = Ner3 {..}
        where
            ner3docTokenBegin = TokenID ner_docTokenBegin
            ner3docTokenEnd = TokenID ner_docTokenEnd
            ner3tokenBegin = TokenID ner_tokenBegin
            ner3tokenEnd = TokenID ner_tokenEnd
            ner3text = LCtext {ltxt = ner_text, llang = lang}
            ner3characterOffsetBegin = ner_characterOffsetBegin
            ner3characterOffsetEnd = ner_characterOffsetEnd
            ner3ner = toNERtag ner_ner

