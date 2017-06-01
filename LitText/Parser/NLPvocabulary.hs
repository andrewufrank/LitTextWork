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
      , module Parser.ProduceLit
      , module Data.RDF.Extension
      , module Uniform.Strings
--      , buchURIx, paraSigl, unPara
--      , PartURI, RDFproperty
--      , NLPproperty (..)
--      , nlpURItext
    ) where

import           CoreNLP.Defs0
--import           CoreNLP.DependencyCodes
import           Data.RDF.Extension      --(PartURI, RDFproperty)
import           Parser.Foundation hiding ((</>), (<.>), (<|>))
import           Parser.ProduceLit     --  (buchURIx, paraSigl)
import           Text.Printf             (printf)
import           Uniform.Strings         hiding ((<|>))
-- import Uniform.StringInfix

nlp = "nlp"::Text
nlpURItext = gerastreeURI </> "nlp_2015" :: PartURI

data NLPproperty = LanguageTag | FileName | Parse | Lemma | Lemma3
          | Pos | WordForm | Nertag | SpeakerTag
          | DependencyType | Dependency
          | SentenceForm
          | Governor | Dependent | DepWordform
          | GovernorWordform | DependentWordform
          deriving (Read, Show, Eq, Enum)
          -- attention: these values will be used with lowercase first letter

instance RDFproperties NLPproperty where
    mkRDFproperty p = RDFproperty $ nlpURItext <#> (toLowerStart . showT $ p)

data NLPtype = Doc | Sentence | Token
    | DepType | Dependence | Mention | Coreference
  deriving (Show, Eq, Enum)

instance RDFtypes NLPtype where
      mkRDFtype p = RDFtype $ nlpURItext <#> (toTitle . showT $ p)

type DocSigl = ParaSigl
unDocSigl = unParaSigl

newtype SentSigl = SentSigl RDFsubj deriving (Show, Eq)
mkSentSigl :: DocSigl  -> SentID0 -> SentSigl
unSentSigl (SentSigl a) = a
-- make the sentence id from buchsigl (docid) and sentnumber
-- mkSentSigl docid sentid =  RDFsubj $ docid <+> "S" <> (formatSentenceID  . unSentID0 $    sentid)
mkSentSigl docsigl sentid =  SentSigl
      . extendSlashRDFsubj  (formatSentenceID  . unSentID0 $    sentid)
      . unDocSigl $ docsigl
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

mkDepSigl :: DepTypeSigl -> Int -> DepSigl
-- make the dependency sigl (these must be numbered)
mkDepSigl deptsigl  i =  DepSigl
      . extendSlashRDFsubj (formatDepID i)   -- is a Text
      . unDepTypeSigl $ deptsigl
    where
        formatDepID  = ("Dep" <>) .s2t . printf ('%' : '0' : '2' : 'd' :[])

-- mkTokenSigl sentid  tok =  RDFsubj $ sentid <+>  "T" <>  (formatTokenID . untid0   $ tok)

--mkCorefID :: CorefID -> Text
--mkCorefID c =  t2oURI <#>  "Coref"<> (coref2text c)
--
--
--
--mkMentionID :: MentionID -> Text
--mkMentionID   c =  t2oURI <#>  "M"<> (coref2text . mid1doc $ c)  <-> (show' . mid1int $ c)

--formatSnippetID ::Int -> Text
---- format an Int to 2 decimals for tokens in sentence
--formatSnippetID  = s2t . printf ('%' : '0' : '2' : 'd' :[])
