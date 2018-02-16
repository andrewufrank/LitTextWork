-----------------------------------------------------------------------------
--
-- Module      :  Parser . NLP Vocabulary
-- Copyright   :  andrew u frank -
--
-- | the list of properties and types used to describe the NLP results
-- addition to the list of Treebank codes imported and exported here
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.NLPvocabulary
    ( module Parser.NLPvocabulary
      , module CoreNLP.Defs0
--      , module Parser.ProduceLit
      , module Data.RDF.Extension
      , module Uniform.Strings
--      , buchURIx, paraSigl
--      , PartURI, RDFproperty
--      , SnipID, unSnipID
--      , NLPproperty (..)
--      , nlpURItext
    ) where

import           CoreNLP.Defs0
import           Data.RDF.Extension      --(PartURI, RDFproperty)
import           Text.Printf             (printf)
import           Uniform.Strings         hiding ((<|>))
import Parser.TextDescriptor hiding ((</>)) -- from Foundation
import Producer.Servers (rdfBase, vocabularyBase)  -- from Foundation

nlp = "nlp"::Text
nlpURItext =  (showT vocabularyBase) </> "nlp_2015" :: PartURI

data NLPproperty = LanguageTag | FileName | Parse | Lemma | Lemma3
          | Pos | PosOrig | WordForm | Ner  | NerOrig |Speaker
          | DependencyType | Dep  | DepOrig
          | SentenceForm
          | Governor | Dependent | DepWordform
          | GovernorWordform | DependentWordform
          | MentionRepresenatative | MentionSentence
          | MentionSentenceStart | MentionSentenceEnd
          | MentionSentenceHead  | MentionSentenceText
          deriving (Read, Show, Eq, Enum)
          -- attention: these values will be used with lowercase first letter
          -- do not capitalize second and following (not DEPorig)

instance RDFproperties NLPproperty where
    mkRDFproperty p = RDFproperty $ nlpURItext <#> (toLowerStart . showT $ p)

data NLPtype = Doc | Snip | Sentence | Token
    | DepType | Dependence | Mention | Coreference
  deriving (Show, Eq, Enum)

instance RDFtypes NLPtype where
      mkRDFtype p = RDFtype $ nlpURItext <#> (toTitle . showT $ p)

type ParaID = Int   -- should be typed?

newtype ParaSigl = ParaSigl RDFsubj
unParaSigl (ParaSigl t) = t

formatParaID :: ParaID -> Text
formatParaID nr =   "P" <> (s2t . printf  ('%' : '0' : '5' : 'd' :[]) $  nr )
-- format to 5 digits
--
--formatLineID :: Int -> Text
--formatLineID nr = "L" <> (s2t . printf  ('%' : '0' : '3' : 'd' :[]) $  nr )
---- format to 3 digits

buchURIx textstate = RDFsubj $ (showT rdfBase)
            <#> authorDir textstate <-> buchName textstate
-- id of buch, part and sentence or page is attached

paraSigl :: TextDescriptor -> ParaNum -> ParaSigl
paraSigl textstate pn = ParaSigl ( extendSlashRDFsubj
                (formatParaID . unparaNum $ pn)
                      ( buchURIx $ textstate)
                      )


-- snip sigl uses the paragraph number of the first paragraph
newtype SnipID  =   SnipID Int  deriving (Show, Read, Eq, Ord)
unSnipID (SnipID i) = i
--
newtype SnipSigl = SnipSigl RDFsubj deriving (Show, Read, Eq)
mkSnipSigl :: ParaSigl   -> SnipID -> SnipSigl
unSnipSigl (SnipSigl a) = a
mkSnipSigl parasigl snipid =  SnipSigl
      . extendSlashRDFsubj  (formatSnipID  . unSnipID $    snipid)
      . unParaSigl $ parasigl
  where
    formatSnipID ::Int -> Text
    -- format an Int to 2 decimals for Snis
    formatSnipID  = ("N" <>) . s2t . printf ('%' : '0' : '2' : 'd' :[])


newtype SentSigl = SentSigl RDFsubj deriving (Show, Eq)
mkSentSigl :: SnipSigl  -> SentID0 -> SentSigl
unSentSigl (SentSigl a) = a
-- make the sentence id from buchsigl (docid) and sentnumber
-- mkSentSigl docid sentid =  RDFsubj $ docid <+> "S" <> (formatSentenceID  . unSentID0 $    sentid)
mkSentSigl docsigl sentid =  SentSigl
      . extendSlashRDFsubj  (formatSentenceID  . unSentID0 $    sentid)
      . unSnipSigl $ docsigl
  where
    formatSentenceID ::Int -> Text
    -- format an Int to 6 decimals for sentences
    formatSentenceID  = ("S" <>) . s2t . printf ('%' : '0' : '6' : 'd' :[])

newtype TokenSigl = TokenSigl RDFsubj deriving (Show, Eq)
unTokenSigl (TokenSigl a) = a

mkTokenSigl :: SentSigl -> TokenID0 -> TokenSigl
-- make the token sigl from sentence id
mkTokenSigl sentsigl  tok =  TokenSigl
      . extendSlashRDFsubj  (formatTokenID  . untid0 $  tok)
      . unSentSigl $ sentsigl
-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
  where
    formatTokenID ::Int -> Text
    -- format an Int to 3 decimals for tokens in sentence
    formatTokenID  = ("T" <>) .s2t . printf ('%' : '0' : '3' : 'd' :[])

newtype DepTypeSigl = DepTypeSigl RDFsubj deriving (Show, Eq)
unDepTypeSigl (DepTypeSigl a) = a

mkDepTypeSigl :: SentSigl -> DepTypeID0 -> DepTypeSigl
-- make the token sigl from sentence id
mkDepTypeSigl sentsigl  did =  DepTypeSigl
      . extendSlashRDFsubj did   -- is a Text
      . unSentSigl $ sentsigl

newtype DepSigl = DepSigl RDFsubj deriving (Show, Eq)
unDepSigl (DepSigl a) = a


mkDepSigl2 :: SentSigl -> Int -> DepSigl
-- make the dependency sigl (these must be numbered)
mkDepSigl2 sentsigl  i =  DepSigl
      . extendSlashRDFsubj (formatDepID i)   -- is a Text
      . unSentSigl $ sentsigl
    where
        formatDepID  = ("Dep" <>) .s2t . printf ('%' : '0' : '2' : 'd' :[])

-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)
type CorefNr = Int
newtype CorefSigl = CorefSigl RDFsubj deriving (Show, Eq)
unCorefSigl (CorefSigl a) = a

mkCorefsigl :: SnipSigl -> CorefNr -> CorefSigl
-- given a snip id produce a corefid with the number given
mkCorefsigl snip c =   CorefSigl
      . extendSlashRDFsubj  (formatCorefID     c)
      . unSnipSigl $ snip
  where
    formatCorefID ::Int -> Text
    -- format an Int to 3 decimals for tokens in sentence
    formatCorefID  = ("Coref" <>) . s2t . printf ('%' : '0' : '3' : 'd' :[])

--      t2oURI <#>  "Coref"<> (coref2text c)

type MentionNr = Int
newtype MentionSigl = MentionSigl RDFsubj deriving (Show, Eq)
unMentionSigl (MentionSigl a) = a

mkMentionsigl :: CorefSigl -> MentionNr -> MentionSigl
-- given a snip id produce a Mentionid with the number given
mkMentionsigl corefsigl c =   MentionSigl
      . extendSlashRDFsubj  (formatMentionID c)
      . unCorefSigl $ corefsigl
  where
    formatMentionID ::Int -> Text
    -- format an Int to 3 decimals for tokens in sentence
    formatMentionID  = ("Mention" <>) . s2t . printf ('%' : '0' : '3' : 'd' :[])


