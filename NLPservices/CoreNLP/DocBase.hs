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
import           Text.Printf             (printf)
import GHC.Generics


newtype  Wordform0 = Wordform0 {word0 :: Text}
            deriving (Show, Read, Eq, Ord, Generic)
-- ^ a single word in the form it is in the text
-- should be language encoded

newtype  Lemma0 = Lemma0 {lemma0 :: Text}
            deriving (Show, Read, Eq, Ord, Generic)
-- ^ a single word as lemma

--newtype TokenID0 = TokenID0 {untid0 :: Int}
--deriving (Show, Read, Eq, Ord, Zeros)
----instance Zeros TokenID0 where zero = TID0 zero
--instance NiceStrings TokenID0 where
--    shownice   = show' . untid0

mkTokenID :: Text -> TokenID
-- to make a sentence id, consist of coll id and number
mkTokenID s = TokenID  $ readNoteT "mkTokenID" s

--newtype  SentID0 = SentID0 {unSentID0 :: Int}
--deriving (Show, Read, Eq, Ord, Zeros)
----instance Zeros SentID0 where zero = SentID0 zero
--instance NiceStrings SentID0 where
--    shownice   = show' . unSentID0

mkSentID :: Text -> SentenceID
-- to make a sentence id, consist of coll id and number
mkSentID s = SentenceID $ readNoteT "mkSentID" s

newtype ParaRelID = ParaRelID  [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype CorefRelID = CorefRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype MentionRelID = MentionRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype DocRelID = DocRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype SentenceRelID = SentenceRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype TokenRelID = TokenRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)
newtype DepRelID = DepRelID [Text]
            deriving (Show, Read, Eq, Ord, Generic)

instance Zeros ParaRelID where zero = ParaRelID zero
instance Zeros CorefRelID where zero = CorefRelID zero
instance Zeros MentionRelID where zero = MentionRelID zero
instance Zeros DocRelID where zero = DocRelID zero
instance Zeros SentenceRelID where zero = SentenceRelID zero
instance Zeros TokenRelID where zero = TokenRelID zero
instance Zeros DepRelID where zero = DepRelID zero

--class AbsID abs0 relID abs1 where
--    addToAbsID :: abs0 -> relID -> abs1
--    -- add a new local relative id to the current abs ID
----instance AbsID

addDep2SentID (SentenceRelID d) s@(DepID s1) = DepRelID $ formatID s : d
addSent2DocID (DocRelID d) s@(SentenceID s1) = SentenceRelID $ formatID s : d
addTok2SentID (SentenceRelID d) s@(TokenID s1) = TokenRelID $ formatID s : d
newtype ParaID = ParaID  Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype CorefID = CorefID Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype MentionID = MentionID Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype DocID = DocID Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype SentenceID = SentenceID Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype TokenID = TokenID Int
            deriving (Show, Read, Eq, Ord, Generic)
newtype DepID = DepID Int
            deriving (Show, Read, Eq, Ord, Generic)

instance Zeros ParaID where zero = ParaID zero
instance Zeros CorefID where zero = CorefID zero
instance Zeros MentionID where zero = MentionID zero
instance Zeros DocID where zero = DocID zero
instance Zeros SentenceID where zero = SentenceID zero
instance Zeros TokenID where zero = TokenID zero
instance Zeros DepID where zero = DepID zero

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

instance FormatID DocID where
--formatSnipID ::Int -> Text
-- format an Int to 2 decimals for Snis
    formatID  = formatID' "Doc" 5 -- "N" <>) . s2t . printf ('%' : '0' : '5' : 'd' :[])
    unID (DocID i) = i


    --
