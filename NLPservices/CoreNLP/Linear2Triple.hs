-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- linearize doc11 and convert to triples
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

module CoreNLP.Linear2Triple
    ( module CoreNLP.Linear2Triple
    , DocAsList (..)
    , rdfBase
--    ,  module CoreNLP.DocNLP_0or1
    ,
    ) where

import           Uniform.Strings
--import CoreNLP.Doc1_absoluteID
import CoreNLP.Doc2ToLinear
import  CoreNLP.Vocabulary as Voc
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
--import qualified NLP.Types.Tags      as NLP
--import              CoreNLP.DEPcodes
--import              CoreNLP.NERcodes
import Data.RDFext.Extension as RDF
import Uniform.Zero
import Uniform.Strings
import Data.Maybe
import GHC.Generics
import LitTypes.ServerNames (rdfBase)

newtype NLPtriple postag = NLPtriple Triple
    deriving (Eq, Ord, Show, Read)
--    deriving newtype Zeros   -- not feasible, probably because it is parametrized?

unNLPtriple (NLPtriple t) = t

class MakeIRI p where
-- make an IRI from a nRelID
    mkIRI ::  PartURI -> p -> PartURI

mkIRI_ :: Text -> Text -> [Text] -> PartURI
-- the internal code
mkIRI_ note base ts = if null ts
        then errorT ["mkIRI with empty list for ", note]
        else PartURI $ base </> (fromJustNote ("intercalate mkIRI  " ++ (t2s note))
                            $ intercalate' "/" . reverse $ ts)

instance MakeIRI DocRelID where
    mkIRI (PartURI base) (DocRelID ts)
            =   mkIRI_ "DocRelID" base ts

instance MakeIRI SentenceRelID where
    mkIRI (PartURI base) (SentenceRelID ts)
            =  mkIRI_ "SentenceRelID" base ts

instance MakeIRI TokenRelID where
    mkIRI (PartURI base) (TokenRelID ts)
            =   mkIRI_ "TokenRelID" base ts

data DocAsTriple   =
    TriType {s::PartURI , ty ::NLPtype}
    | TriTextL  {s::PartURI , p::NLPproperty, te ::Text}  -- should be language coded
    | TriText   {s::PartURI , p::NLPproperty, te ::Text}  -- should not be language coded
    | TriText2   {s::PartURI , pp::RDFproperty, te ::Text}  -- should not be language coded
--    | TriRel   {s::PartURI , p::NLPproperty, o ::PartURI}
    | TriRel2   {s::PartURI , pp::RDFproperty, o ::PartURI}
    | TriList {s::PartURI, p::NLPproperty, os :: [Text]}
    | TriZero {}

    deriving (Show, Read, Eq, Ord, Generic)

instance Zeros (DocAsTriple  ) where zero = TriZero

makeTriple :: (Show postag) =>  PartURI -> DocAsList postag -> [DocAsTriple ]

makeTriple base DocAsList {..} = [TriType (mkIRI base d3id)  Voc.Doc]

makeTriple base SentenceLin{..} = [TriType s Voc.Sentence
                               , maybe zero (TriText s  Voc.SentenceParse) s3parse]
                               -- sentence form not in the data
    where s = mkIRI base s3id

makeTriple base DependenceLin{..} = [TriRel2 s (mkRDFproperty d3type) o]
        -- uses the correct nlp prefix because d3type is a DepType
        -- how to find the places where the original type is not parsed?
        -- find earlier ??
    where   s = mkIRI base d3govid
            o = mkIRI base d3depid

makeTriple base MentionLin{..} = [TriRel2 s (mkRDFproperty Voc.Mentions) o]
        --  s is a refers to o (mentions o
    where   s = mkIRI base ment3Head
            o = mkIRI base ment3Ment

makeTriple base TokenLin{..} = [TriType s Voc.Token
                               ,  TriTextL s  WordForm (word0 t3word)
                               , TriTextL s Lemma3 (lemma0 t3lemma)
                               , TriText2 s (mkRDFproperty Voc.Pos) (showT t3pos)
                               , TriList s Voc.Ner (map showT t3ner)
                               , TriList s Voc.Speaker ( map showT t3speaker)
                               ]

    where s = mkIRI base t3id


                --
