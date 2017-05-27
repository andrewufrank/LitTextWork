-----------------------------------------------------------------------------
--
-- Module      :  Defs0
--
-- | the data types etc necessary to read the XML files
-- produced by Stanford CoreNLP
-- captures all info in the XML output but does no furthre processing
-- nor reading (except for the id)
-- the structure is a nested data structure, exactly parallel to the produced xml

-- optional fields are lists .. to conform to HXT
-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveAnyClass
    #-}

module CoreNLP.Defs0 (
        module CoreNLP.Defs0
--        , module CoreNLP.POScodes
        , readSpeakerTag
--        , module CoreNLP.NERcodes -- import separately when needed
            , DepCode (..), readDepCodes, hasDepCode
            , DepCode1 (..), DepCode2 (..)
--        , module CoreNLP.DEPcodes  -- import separately when needed
        -- ,readDocString
        )  where

import              Uniform.Strings
import Uniform.Zero
import CoreNLP.POScodes
import CoreNLP.NERcodes
import CoreNLP.DEPcodes
-- import              Uniform.FileIO
--import              LitNLP.Tools
--import              CoreNLP.DependencyCodes
-- import           Text.XML.HXT.Core       hiding (when)


newtype  Wordform0 = Wordform0 {word0 :: Text} deriving (Show, Read, Eq, Ord, Zeros)
-- ^ a single word in the form it is in the text

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

data Doc0 = Doc0 {docSents:: [Sentence0]
                 , docCorefs :: [Coref0]
                       } deriving (Show,  Eq, Zeros)
-- instance Zeros Doc0 where zero = Doc0 [] []

data Sentence0 = Sentence0 {sid :: SentID0
                        , sparse :: Text  -- could be the parse tree
                        , stoks :: [Token0]
                        , sdeps :: Maybe DependenceType0
                        -- should be only one of none
                        } deriving (Show,  Eq)

type DepTypeID0 = Text

data DependenceType0 = DependenceType0 { dtt :: DepTypeID0
            , dtd :: [Dependence0]
            } deriving (Show, Read, Eq, Zeros)

data Token0 = Token0 { tid :: TokenID0
                    , tword :: Wordform0
                    , tlemma :: Lemma0
                    , tbegin, tend :: Int  -- not used
                    , tpos :: Pos -- is Connl.Tag -- Text -- is Conll.Tag defined ind DependencyCodes -- Penn Treebank
                    , tpostt :: Text -- the pos from the tree tagger
                    , tner :: [Text] -- String
                    , tspeaker :: [SpeakerTag] -- Text -- String
                    }   deriving (Show, Eq)

data Dependence0 = Dependence0 {dtype :: DepCode -- Text -- String
                        , dgov :: DependencePart0
                        , ddep :: DependencePart0
                        }   deriving (Show, Read, Eq)

data DependencePart0 = DP0 { did :: TokenID0  -- word number in sentence
                        , dword :: Wordform0  -- is word from text, not lemma
                            }   deriving (Show, Read, Eq)

data Coref0 = Coref0 {corefMents:: [Mention0]
        }
  deriving (Show, Read, Eq)

data Mention0 = Mention0 {mentRep :: Text -- Bool
        , mentSent :: SentID0
        , mentStart, mentEnd :: TokenID0 -- not used ??
        , mentHead :: TokenID0  -- not used
        , mentText :: Text  -- multiple words - not yet used
        }
  deriving (Show, Read, Eq)
