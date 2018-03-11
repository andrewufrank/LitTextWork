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
--    , DocAsLinear (..)
--    ,  module CoreNLP.DocNLP_0or1
    ,
    ) where

import           Uniform.Strings
--import CoreNLP.Doc1_absoluteID
import CoreNLP.Doc2ToLinear
import qualified CoreNLP.Vocabulary as Voc
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
--import qualified NLP.Types.Tags      as NLP
--import              CoreNLP.DEPcodes
--import              CoreNLP.NERcodes
import Data.RDFext.Extension
import Uniform.Zero
import Uniform.Strings
import Data.Maybe
import GHC.Generics

newtype NLPtriple postag = NLPtriple Triple deriving (Eq, Ord, Show, Read)
unNLPtriple (NLPtriple t) = t

class MakeIRI p where
-- make an IRI from a nRelID
    mkIRI ::  PartURI -> p -> PartURI
instance MakeIRI DocRelID where
    mkIRI (PartURI base) (DocRelID ts)
            = PartURI (base </>
                    (fromJustNote "intercalate mkIRI" $ intercalate' "/" ts))

instance MakeIRI SentenceRelID where
    mkIRI (PartURI base) (SentenceRelID ts)
            = PartURI (base </>
                    (fromJustNote "intercalate mkIRI" $ intercalate' "/" ts))

data DocAsTriple postag =
    TriType {s::PartURI , ty ::NLPtype}
    | TriText

makeTriple :: PartURI -> DocAsList postag -> [DocAsTriple postag]

makeTriple base DocLin {..} = [TriType (mkIRI base d3id)  Voc.Doc]

makeTriple base SentenceLin{..} = [TriType s Voc.Sentence
                                TriText s SentenceForm ]
    where s = mkIRI base s3id




                --
