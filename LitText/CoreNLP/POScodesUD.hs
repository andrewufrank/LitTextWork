{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for UD  -- the table is lifted
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
        , DeriveGeneric
        #-}

module CoreNLP.POScodesUD (module CoreNLP.POScodesUD
--        , module NLP.Corpora.Conll
        )
         where

import GHC.Generics
import Data.Serialize (Serialize)

import           Test.Framework

import Uniform.Zero
import Uniform.Strings
import Uniform.Error

import qualified NLP.Corpora.Conll      as Conll

import qualified NLP.Types.Tags as NLPtypes
--import      NLP.Corpora.Conll
--import      NLP.Corpora.Conll   as Conll

type PosTagEng = Conll.Tag   -- renames the ConllTag
instance CharChains2 PosTagEng Text

data PosTagUD =   -- copied from http://universaldependencies.org/u/pos/
    ADJ | -- adjective
    ADP | -- adposition
    ADV | -- adverb
    AUX | -- auxiliary
    CCONJ | -- coordinating conjunction
    DET | -- determiner
    INTJ | -- interjection
    NOUN | -- noun
    NUM | -- numeral
    PART | -- particle
    PRON | -- pronoun
    PROPN | -- proper noun
    PUNCT | -- punctuation
    SCONJ | -- subordinating conjunction
    SYM | -- symbol
    VERB | -- verb
    X  -- other
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag PosTagUD where
--parseTag :: Text -> PosTag
    parseTag txt = case readTag txt of
                       Left  _ -> X
                       Right t -> t

readTag :: Text -> ErrOrVal PosTagUD
readTag txt = maybe2errorP . read . t2s $ txt

maybe2errorP  :: Maybe a -> ErrOrVal a
maybe2errorP Nothing = Left "readTag PosTagUD 34232"
maybe2errorP (Just a) = Right a

instance Serialize PosTagUD

instance CharChains2 PosTagUD Text

instance Zeros PosTagUD where zero = X
--type Unk = Conll.Unk



