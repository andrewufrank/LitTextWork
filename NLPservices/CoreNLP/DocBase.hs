-----------------------------------------------------------------------------
--
-- Module      :  Defs0
--
-- | the base data types for the Doc2 which results from reading the JSONflag
-- output from NLP

-- all data has 0 suffix
-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveAnyClass
    #-}

module CoreNLP.DocBase (
        module CoreNLP.DocBase
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


newtype  Wordform0 = Wordform0 {word0 :: Text} deriving (Show, Read, Eq, Ord, Zeros)
-- ^ a single word in the form it is in the text
-- should be language encoded

newtype  Lemma0 = Lemma0 {lemma0 :: Text} deriving (Show, Read, Eq, Ord, Zeros)
-- ^ a single word as lemma

newtype TokenID0 = TokenID0 {untid0 :: Int} deriving (Show, Read, Eq, Ord, Zeros)
--instance Zeros TokenID0 where zero = TID0 zero
instance NiceStrings TokenID0 where
    shownice   = show' . untid0

mkTokenID :: Text -> TokenID0
-- to make a sentence id, consist of coll id and number
mkTokenID s = TokenID0 $ readNoteT "mkTokenID" s

newtype  SentID0 = SentID0 {unSentID0 :: Int} deriving (Show, Read, Eq, Ord, Zeros)
--instance Zeros SentID0 where zero = SentID0 zero
instance NiceStrings SentID0 where
    shownice   = show' . unSentID0

mkSentID :: Text -> SentID0
-- to make a sentence id, consist of coll id and number
mkSentID s = SentID0 $ readNoteT "mkSentID" s

data Token0 postag = Token0 { tid :: TokenID0
                    , tword :: Wordform0
                    , tlemma :: Lemma0
                    , tbegin, tend :: Int  -- not used
                    , tpos :: postag --  the pos tag recognized
                    , tposOrig :: Text -- the pos tag received
                    , tpostt :: Text -- the pos from the tree tagger
                    , tner :: [NERtag] -- [Text] -- String
                    , tspeaker :: [SpeakerTag] -- Text -- String
                    }   deriving (Read, Show, Eq)


type DepTypeID0 = Text

data DependenceType0 = DependenceType0 { dtt :: DepTypeID0
                -- not used - whatever the last depType produced is taken
            , dtd :: [Dependence0]
            } deriving (Show, Read, Eq, Zeros)


data Dependence0 = Dependence0 {dtype :: DepCode -- Text -- String
                        , dorig :: Text -- the value given in the XML
                        , dgov :: DependencePart0
                        , ddep :: DependencePart0
                        }   deriving (Show, Read, Eq)

data DependencePart0 = DP0 { did :: TokenID0  -- word number in sentence
                        , dword :: Wordform0  -- is word from text, not lemma
                            }   deriving (Show, Read, Eq)


