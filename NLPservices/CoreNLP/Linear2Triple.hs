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

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

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

newtype NLPtriple postag = NLPtriple Triple deriving (Eq, Ord, Show, Read)
unNLPtriple (NLPtriple t) = t

class MakeIRI p where
-- make an IRI from a nRelID
    mkIRI ::  PartURI -> p -> PartURI
instance MakeIRI DocRelID where
    mkIRI (PartURI base) (DocRelID ts)
            = PartURI (base </>
                    (fromJustNote "intercalate mkIRI DocRelID"
                            $ intercalate' "/" . reverse $ ts))

instance MakeIRI SentenceRelID where
    mkIRI (PartURI base) (SentenceRelID ts)
            = PartURI (base </>
                    (fromJustNote "intercalate mkIRI SentenceRelID"
                            $ intercalate' "/" . reverse $ ts))

instance MakeIRI TokenRelID where
    mkIRI (PartURI base) (TokenRelID ts)
            = PartURI (base </>
                    (fromJustNote "intercalate mkIRI TokenRelID"
                            $ intercalate' "/" . reverse $ ts))

data DocAsTriple postag =
    TriType {s::PartURI , ty ::NLPtype}
    | TriTextL  {s::PartURI , p::NLPproperty, te ::Text}  -- should be language coded
    | TriText   {s::PartURI , p::NLPproperty, te ::Text}  -- should not be language coded
    | TriRel   {s::PartURI , p::NLPproperty, o ::PartURI}  -- should not be language coded
    | TriRel2   {s::PartURI , pp::RDFproperty, o ::PartURI}  -- should not be language coded
    | TriList {s::PartURI, p::NLPproperty, os :: [Text]}
    | TriZero {}

    deriving (Show, Read, Eq, Ord, Generic)

instance Zeros (DocAsTriple postag) where zero = TriZero

makeTriple :: (Show postag) =>  PartURI -> DocAsList postag -> [DocAsTriple postag]

makeTriple base DocAsList {..} = [TriType (mkIRI base d3id)  Voc.Doc]

makeTriple base SentenceLin{..} = [TriType s Voc.Sentence
                               , maybe zero (TriText s  Voc.SentenceParse) s3parse]
                               -- sentence form not in the data
    where s = mkIRI base s3id

makeTriple base DependenceLin{..} = [TriRel2 s (mkRDFproperty d3type) o]
        -- how to find the places where the original type is not parsed?
        -- find earlier ??
    where   s = mkIRI base d3govid
            o = mkIRI base d3depid

makeTriple base MentionLin{..} = [TriRel s Voc.Mentions o]
        --  s is a refers to o (mentions o
    where   s = mkIRI base ment3Head
            o = mkIRI base ment3Ment

makeTriple base TokenLin{..} = [TriType s Voc.Token
                               ,  TriTextL s  WordForm (word0 t3word)
                               , TriTextL s Lemma3 (lemma0 t3lemma)
                               , TriText s Voc.Pos (showT t3pos)
                               , TriList s Voc.Ner (map showT t3ner)
                               , TriList s Voc.Speaker ( map showT t3speaker)
                               ]

    where s = mkIRI base t3id


                --
