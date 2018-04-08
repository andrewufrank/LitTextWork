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
    , RecordWildCards
    , DeriveGeneric
    , DeriveAnyClass     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LitText.CoreNLP.Vocabulary
    ( module LitText.CoreNLP.Vocabulary
      , module LitText.CoreNLP.DocBase
      , RDFsubj (..), PartURI (..)
    ) where
import LitText.CoreNLP.DocBase hiding (Snip)
import Uniform.Http
--import Data.RDFext

data NLPproperty = LanguageTag | FileName | Parse | Lemma
            | Lemma3
          | Pos | TokenPOSorig | WordForm | Ner
          | TokenNERorig | TokenSpeaker
--          | TokenPosTT
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

instance RDFtypes NLPproperty where
    mkRDFproperty p =  mkRDFproperty $ append2IRI nlpIRItext (toLowerStart . showT $ p)

instance RDFtypes DepCode where
    mkRDFproperty c = mkRDFproperty $  append2IRI nlpUDEPtext (toLower' . fromDEPtag $ c)
-- the dep codes used as rdf properties are in a separate prefix

            -- should be changed to 2017
data NLPtype =  Snip
    | Sentence | Token
--    | DepType
    | Dependence | Mention  -- | Coreference
    | NERentity
--    | MentionChain
  deriving (Show, Read, Eq, Ord, Generic)

instance RDFtypes NLPtype where
      mkRDFtype p = mkRDFtype $ append2IRI nlpIRItext  (toTitle . showT $ p)


newtype ParaSigl = ParaSigl RDFsubj deriving (Show, Read, Eq, Ord)
unParaSigl (ParaSigl t) = t

paraSigl :: TextDescriptor -> ParaID -> ParaSigl
paraSigl textstate pn = ParaSigl $ append2IRIwithSlash
               ( buchURIx $ textstate)  (formatID  pn)




buchURIx textstate = mkRDFsubj $ append2IRI rdfBase
            (authorDir textstate <-> buchName textstate)
-- id of buch, part and sentence or page is attached



--
newtype SnipSigl = SnipSigl RDFsubj deriving (Show, Read, Eq, Generic, Zeros)
--instance Zeros SnipSigl where zero = SnipSigl zero


mkSnipSigl :: ParaSigl   -> SnipID -> SnipSigl
unSnipSigl (SnipSigl a) = a
mkSnipSigl parasigl snipid =  SnipSigl
     $ append2IRIwithSlash
        (unParaSigl $ parasigl)  (formatID  snipid)

  where


newtype SentSigl = SentSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
mkSentSigl :: SnipSigl  -> SentenceID -> SentSigl
unSentSigl (SentSigl a) = a
-- make the sentence id from buchsigl (docid) and sentnumber
-- mkSentSigl docid sentid =  RDFsubj $ docid <+> "S" <> (formatSentenceID  . unSentID0 $    sentid)
mkSentSigl docsigl sentid =  SentSigl
      $ append2IRIwithSlash
        ( unSnipSigl $ docsigl ) (formatID   sentid)

  where

newtype TokenSigl = TokenSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unTokenSigl (TokenSigl a) = a

mkTokenSigl :: SentSigl -> TokenID -> TokenSigl
-- make the token sigl from sentence id
mkTokenSigl sentsigl  tok =  TokenSigl
      $ append2IRIwithSlash
            (unSentSigl $ sentsigl) (formatID tok)

-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
  where

--newtype DepTypeSigl = DepTypeSigl RDFsubj deriving (Show, Eq)
--unDepTypeSigl (DepTypeSigl a) = a
--
--mkDepTypeSigl :: SentSigl -> DepID -> DepTypeSigl
---- make the token sigl from sentence id
--mkDepTypeSigl sentsigl  did =  DepTypeSigl
--      . append2IRIwithSlash did   -- is a Text
--      . unSentSigl $ sentsigl

newtype DepSigl = DepSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unDepSigl (DepSigl a) = a


mkDepSigl2 :: SentSigl -> DepID  -> DepSigl
-- make the dependency sigl (these must be numbered)
mkDepSigl2 sentsigl  i =  DepSigl
      $ append2IRIwithSlash
         (unSentSigl $ sentsigl) (formatID i)   -- is a Text

    where

-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
newtype CorefSigl = CorefSigl RDFsubj deriving (Show, Read, Eq, Ord, Generic)
unCorefSigl (CorefSigl a) = a

mkCorefsigl :: SnipSigl -> CorefID -> CorefSigl
-- given a snip id produce a corefid with the number given
mkCorefsigl snip c =   CorefSigl
        $ append2IRIwithSlash
         (unSnipSigl  snip) (formatID c)


type MentionNr = Int
newtype MentionSigl = MentionSigl RDFsubj deriving  (Show, Read, Eq, Ord, Generic)
unMentionSigl (MentionSigl a) = a

mkMentionsigl :: CorefSigl -> MentionID -> MentionSigl
-- given a snip id produce a Mentionid with the number given
mkMentionsigl corefsigl c =   MentionSigl
        $ append2IRIwithSlash
         (unCorefSigl  corefsigl) (formatID c)


