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
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
-- ^ a single word in the form it is in the text
-- should be language encoded

newtype  Lemma0 = Lemma0 {lemma0 :: Text}
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
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

newtype ParaID = ParaID  Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype CorefID = CorefID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype MentionID = MentionID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype SnipID = SnipID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype SentenceID = SentenceID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype TokenID = TokenID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
newtype DepID = DepID Int
            deriving (Show, Read, Eq, Ord, Generic, Zeros)



class FormatID i where
    formatID :: i -> Text
    unID :: i -> Int

formatInt :: Int -> Int -> Text
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
