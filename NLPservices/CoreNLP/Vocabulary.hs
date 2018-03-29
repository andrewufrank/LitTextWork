-----------------------------------------------------------------------------
--
-- Module      :  Parser . NLP Vocabulary
-- Copyright   :  andrew u frank -
--
-- | the list of properties and types used to describe the NLP results
-- addition to the list of Treebank codes imported and exported here
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module CoreNLP.Vocabulary
    ( module CoreNLP.Vocabulary
      , module CoreNLP.DocBase
--      , module Data.RDFext.Extension
--      , module Uniform.Strings
      , RDFsubj (..), PartURI (..), rdfBase
--      , SnipSigl
    ) where

--import           CoreNLP.DocBase
--import           Data.RDFext.Extension  -- (RDFproperty)
--import           Uniform.Strings         hiding ((<|>))
--import LitTypes.TextDescriptor hiding ((</>)) -- from Foundation
--import LitTypes.ServerNames
--import LitTypes.TextDescriptor
--import LitTypes.TextDescriptor (SnipSigl)
--import CoreNLP.DEPcodes (DepCode(..))
import CoreNLP.DocBase
--import Data.RDFext.Extension
--import GHC.Generics

data NLPproperty = LanguageTag | FileName | Parse | Lemma
            | TokenLemma3
          | TokenPOS | TokenPOSorig | TokenWordForm | TokenNER
          | TokenNERorig | TokenSpeaker | TokenPosTT
--          | DependencyType   -- in CoreNLPxml is the best (last) selected
          -- | Dep  | DepOrig -- not used in nlp_2017
          -- check separately that no unrecognized codes are occuring?
          | SentenceForm | SentenceParse
          | DepGovernor | DepOrigin | DepDependent  -- | DepWordform
          | DepGovGloss | DepDepGloss
--          GovernorWordform | DependentWordform
          | Mentions  -- s is a mention of representative o
          | MentionRepresentative | MentionSentence
          | MentionStart | MentionEnd
          | MentionHead  | MentionText
          | MentionType
          | MentionGender | MentionNumber | MentionAnimacy
          | MentTokenRelID
          | TokenBegin | TokenEnd
          | NerTokenEnd | NerTokenEnd2 | NerTokenBegin2
          | NerType | NerText
            -- when are these in entitymentions different ?
          deriving (Read, Show, Eq, Enum)
          -- attention: these values will be used with lowercase first letter
          -- do not capitalize second and following (not DEPorig)

instance RDFproperties NLPproperty where
    mkRDFproperty p = RDFproperty $ unPartURI nlpURItext <#> (toLowerStart . showT $ p)

instance RDFproperties DepCode where
    mkRDFproperty c = RDFproperty $ unPartURI nlpURItext <#> (toLower' . fromDEPtag $ c)

            -- should be changed to 2017
data NLPtype = Doc  -- | Snip
    | Sentence | Token
--    | DepType
    | Dependence | Mention  -- | Coreference
    | NERentity
--    | MentionChain
  deriving (Show, Read, Eq, Ord, Generic)

instance RDFtypes NLPtype where
      mkRDFtype p = RDFtype $ unPartURI nlpURItext <#> (toTitle . showT $ p)


newtype ParaSigl = ParaSigl RDFsubj deriving (Show, Read, Eq, Ord)
unParaSigl (ParaSigl t) = t

paraSigl :: TextDescriptor -> ParaID -> ParaSigl
paraSigl textstate pn = ParaSigl ( extendSlashRDFsubj
                (formatID  pn)
                      ( buchURIx $ textstate)
                  )


buchURIx textstate = RDFsubj $ (unPartURI rdfBase)
            <#> authorDir textstate <-> buchName textstate
-- id of buch, part and sentence or page is attached



--
newtype SnipSigl = SnipSigl RDFsubj deriving (Show, Read, Eq)
instance Zeros SnipSigl where zero = SnipSigl zero


mkSnipSigl :: ParaSigl   -> SnipID -> SnipSigl
unSnipSigl (SnipSigl a) = a
mkSnipSigl parasigl snipid =  SnipSigl
      . extendSlashRDFsubj  (formatID  snipid)
      . unParaSigl $ parasigl
  where


newtype SentSigl = SentSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
mkSentSigl :: SnipSigl  -> SentenceID -> SentSigl
unSentSigl (SentSigl a) = a
-- make the sentence id from buchsigl (docid) and sentnumber
-- mkSentSigl docid sentid =  RDFsubj $ docid <+> "S" <> (formatSentenceID  . unSentID0 $    sentid)
mkSentSigl docsigl sentid =  SentSigl
      . extendSlashRDFsubj  (formatID   sentid)
      . unSnipSigl $ docsigl
  where

newtype TokenSigl = TokenSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unTokenSigl (TokenSigl a) = a

mkTokenSigl :: SentSigl -> TokenID -> TokenSigl
-- make the token sigl from sentence id
mkTokenSigl sentsigl  tok =  TokenSigl
      . extendSlashRDFsubj  (formatID tok)
      . unSentSigl $ sentsigl
-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
  where

--newtype DepTypeSigl = DepTypeSigl RDFsubj deriving (Show, Eq)
--unDepTypeSigl (DepTypeSigl a) = a
--
--mkDepTypeSigl :: SentSigl -> DepID -> DepTypeSigl
---- make the token sigl from sentence id
--mkDepTypeSigl sentsigl  did =  DepTypeSigl
--      . extendSlashRDFsubj did   -- is a Text
--      . unSentSigl $ sentsigl

newtype DepSigl = DepSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unDepSigl (DepSigl a) = a


mkDepSigl2 :: SentSigl -> DepID  -> DepSigl
-- make the dependency sigl (these must be numbered)
mkDepSigl2 sentsigl  i =  DepSigl
      . extendSlashRDFsubj (formatID i)   -- is a Text
      . unSentSigl $ sentsigl
    where

-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
newtype CorefSigl = CorefSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unCorefSigl (CorefSigl a) = a

mkCorefsigl :: SnipSigl -> CorefID -> CorefSigl
-- given a snip id produce a corefid with the number given
mkCorefsigl snip c =   CorefSigl
      . extendSlashRDFsubj  (formatID     c)
      . unSnipSigl $ snip

type MentionNr = Int
newtype MentionSigl = MentionSigl RDFsubj deriving  (Show, Read, Eq, Ord, Generic)
unMentionSigl (MentionSigl a) = a

mkMentionsigl :: CorefSigl -> MentionID -> MentionSigl
-- given a snip id produce a Mentionid with the number given
mkMentionsigl corefsigl c =   MentionSigl
      . extendSlashRDFsubj  (formatID c)
      . unCorefSigl $ corefsigl


