-----------------------------------------------------------------------------
--
-- Module      :  Defs0
--
-- | the base data types for the
-- output from NLP


-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , TypeSynonymInstances
    , MultiParamTypeClasses
    , DerivingStrategies
    #-}

module LitText.CoreNLP.DocBase (
        module LitText.CoreNLP.DocBase
        ,  SpeakerTags (..)
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
         , LCtext (..)
         , module GHC.Generics
         , module Uniform.Zero
         , module Uniform.Strings
         , module LitText.Foundation
        )  where

import              Uniform.Strings
import Uniform.Zero
import   NLP.TagSets.Conll
import              NLP.TagSets.DEPcodes
import              NLP.TagSets.NERcodes
import              NLP.TagSets.SpeakerTags
import           Text.Printf             (printf)
import GHC.Generics
import LitText.Foundation

newtype  Wordform0 = Wordform0 {word0 :: LCtext}
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
--            deriving newtype Zeros
-- ^ a single word in the form it is in the text
-- should be language encoded

--instance Zeros Wordform0 where zero =  Wordform0 zero

newtype  Lemma0 = Lemma0 {lemma0 :: LCtext}
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
-- ^ a single word as lemma
--instance Zeros Lemma0 where zero =  Lemma0 zero


mkTokenID :: Text -> TokenID
-- to make a sentence id, consist of coll id and number
mkTokenID s = TokenID  $ readNoteT "mkTokenID" s

--newtype  SentID0 = SentID0 {unSentID0 :: Int}

mkSentID :: Text -> SentenceID
-- to make a sentence id, consist of coll id and number
mkSentID s = SentenceID $ readNoteT "mkSentID" s

newtype ParaRelID = ParaRelID  [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype CorefRelID = CorefRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype MentionRelID = MentionRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype SnipRelID = SnipRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype SentenceRelID = SentenceRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype TokenRelID = TokenRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype DepRelID = DepRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros ParaRelID where zero = ParaRelID zero
--instance Zeros CorefRelID where zero = CorefRelID zero
--instance Zeros MentionRelID where zero = MentionRelID zero
--instance Zeros SnipRelID where zero = SnipRelID zero
--instance Zeros SentenceRelID where zero = SentenceRelID zero
--instance Zeros TokenRelID where zero = TokenRelID zero
--instance Zeros DepRelID where zero = DepRelID zero


addDep2SentID (SentenceRelID d) s@(DepID s1) = DepRelID $ formatID s : d
addSent2DocID (SnipRelID d) s@(SentenceID s1) = SentenceRelID $ formatID s : d
addTok2SentID (SentenceRelID d) s@(TokenID s1) = TokenRelID $ formatID s : d
newtype ParaID = ParaID  Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype CorefID = CorefID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype MentionID = MentionID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
--newtype SnipID = SnipID Int
--            deriving (Show, Read, Eq, Ord, Generic)
newtype SentenceID = SentenceID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype TokenID = TokenID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype DepID = DepID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros ParaID where zero = ParaID zero
--instance Zeros CorefID where zero = CorefID zero
--instance Zeros MentionID where zero = MentionID zero
----instance Zeros SnipID where zero = SnipID zero
--instance Zeros SentenceID where zero = SentenceID zero
--instance Zeros TokenID where zero = TokenID zero
--instance Zeros DepID where zero = DepID zero

class FormatID i where
    formatID :: i -> Text
    unID :: i -> Int

formatInt :: Int -> Int -> Text
-- probably not required ??
formatInt n  = s2t . case n of
        6 -> printf  ('%' : '0' : '6' : 'd' :[])
        5 ->  printf  ('%' : '0' : '5' : 'd' :[])
        3 -> printf  ('%' : '0' : '3' : 'd' :[])
        2 ->  printf  ('%' : '0' : '2' : 'd' :[])

formatID' :: FormatID a =>   Text -> Int -> a -> Text
formatID' t i =  append  t .  formatInt i . unID

instance FormatID ParaID where
--  format to 5 digits
    formatID   =  formatID' "P" 5 -- append  "P" .  formatInt 5 . unID
    unID (ParaID i) = i

--formatLineID :: Int -> Text
--formatLineID nr = "L" <> (s2t . printf  ('%' : '0' : '3' : 'd' :[]) $  nr )
---- format to 3 digits
instance FormatID MentionID where
--formatMentionID ::MentionID  -> Text
-- format an Int to 3 decimals for tokens in sentence
    formatID  =  formatID' "Mention" 3 -- <>  formatInt3 $ unID
    unID (MentionID i) = i

instance FormatID CorefID where
--formatCorefID ::Int -> Text
-- format an Int to 3 decimals for tokens in sentence
    formatID  =  formatID' "Coref" 3 -- <>  formatInt3 $ unID
    unID (CorefID i) = i

instance FormatID DepID where
    formatID  = formatID' "Dep" 2
    unID (DepID i) = i

instance FormatID TokenID where
--formatTokenID ::Int -> Text
-- format an Int to 3 decimals for tokens in sentence
    formatID  = formatID' "T" 3 -- <>) .s2t . printf ('%' : '0' : '3' : 'd' :[])
    unID(TokenID i) = i

instance FormatID SentenceID where
--    formatSentenceID ::Int -> Text
-- format an Int to 6 decimals for sentences
    formatID  = formatID' "S" 6 -- <>) . s2t . printf ('%' : '0' : '6' : 'd' :[])
    unID (SentenceID i) = i

instance FormatID SnipID where
--formatSnipID ::Int -> Text
-- format an Int to 2 decimals for Snis
    formatID  = formatID' "Snip" 5 -- "N" <>) . s2t . printf ('%' : '0' : '5' : 'd' :[])
    unID (SnipID i) = i


    --
