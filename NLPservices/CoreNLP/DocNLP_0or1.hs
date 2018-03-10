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
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CoreNLP.DocNLP_0or1 (
        module CoreNLP.DocNLP_0or1
        , module CoreNLP.DocBase
--        , module CoreNLP.POScodes
        ,  SpeakerTags (..)
--        , module CoreNLP.NERcodes -- import separately when needed
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
--        , module CoreNLP.DEPcodes  -- import separately when needed
        -- ,readDocString
        )  where

import              Uniform.Strings
import Uniform.Zero
import   NLP.Corpora.Conll
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import CoreNLP.DocBase
import GHC.Generics

class To1 postag a2 a1 where
-- convert to the 1 or 0 records
    to1 :: postag -> a2 -> a1




data Doc1 postag = Doc1 {doc1Sents:: [Sentence1 postag]
                 , doc1Corefs :: Maybe Coreferences1   -- only one
                       } deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros (Doc1 postag) where zero = Doc1 [] zero

data Sentence1 postag = Sentence1 {s1id :: SentenceID
                        , s1parse :: Maybe Text  -- the parse tree
                        , s1toks :: [Token0 postag]
                        , s1deps :: Maybe [Dependence1]
                        -- should be only one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        }  deriving (Show, Read, Eq, Ord, Generic, Zeros)


data Dependence1 = Dependence1 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID
                        , d1depid :: TokenID
                        , d1govGloss :: Text
                        , d1depGloss :: Text
                        } deriving (Show, Read, Eq, Ord, Generic, Zeros)


--instance Zeros Dependence1  where
--        zero = Dependence1 zero zero zero zero zero zero

data Coreferences1 = Coreferences1 {coChains:: [MentionChain1]}
                deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros Coreferences1 where zero = Coreferences1 []

data MentionChain1 = MentionChain1 [Mention1]
        deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros (MentionChain1) where zero = MentionChain1 []

data Mention1 = Mention1 {mentRep ::  Bool -- , indicates the representative mention
        , mentSent :: SentenceID
        , mentStart, mentEnd :: TokenID -- not used ??
        , mentHead :: TokenID  -- the head of the mention
        , mentText :: Text  -- multiple words, the actual mention - not yet used
        }
  deriving (Show, Read, Eq, Ord, Generic, Zeros)
--instance Zeros Mention1 where zero = Mention1 False zero zero zero zero zero

instance Zeros Bool where zero = False

data Token0 postag = Token0 { tid :: TokenID
                    , tword :: Wordform0
                    , tlemma :: Lemma0
                    , tbegin, tend :: Int  -- not used
                    , tpos :: postag --  the pos tag recognized
                    , tposOrig :: Text -- the pos tag received
                    , tpostt :: Text -- the pos from the tree tagger
                    , tner :: [NERtag] -- [Text] -- String
                    , tspeaker :: [SpeakerTag] -- Text -- String
                    }   deriving (Show, Read, Eq, Ord, Generic, Zeros)


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
                        } deriving   (Show, Read, Eq, Ord, Generic, Zeros)

data DependencePart0 = DP0 { did :: TokenID  -- word number in sentence
                        , dword :: Wordform0  -- is word from text, not lemma
                            } deriving  (Show, Read, Eq, Ord, Generic, Zeros)


