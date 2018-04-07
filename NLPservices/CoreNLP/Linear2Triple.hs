-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- linearize doc11 and convert to triples
-- including all data, except entitymentions
-- convert lin5 to trips6
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
    , Triple

    ) where

import           Uniform.Strings
import CoreNLP.Doc2ToLinear
import qualified CoreNLP.Vocabulary as Voc
import Uniform.Zero
import Uniform.Strings
import Data.Maybe
import GHC.Generics
import LitText.Foundation
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD
import NLP.TagSets.NERcodes (fromNERtag)

toTriple ::  Show postag => postag -> RDFsubj
                    ->  [DocAsList postag] -> [DocAsTriple ]
toTriple postag rdfbase ds  =  concat r
    where r =  map (makeTriple postag rdfbase) ds :: [[DocAsTriple ]]

--toTripleUD :: PartURI ->   [DocAsList UD.POStag] -> [DocAsTriple ]
--toTripleUD rdfbase ds  =  concat r
--    where r =  map (makeTriple rdfbase) ds :: [[DocAsTriple ]]

newtype NTtext = NT Text deriving (Eq, Ord, Read, Show)
-- ^ a special wrap to separate NT encoded text
unNT (NT t) = t
instance Zeros NTtext where zero = NT zero

toNT ::     [DocAsTriple ] -> NTtext
toNT ds  =  NT t
    where
        r =  map (makeRDFnt ) ds :: [[Triple]]
        t = unlines' . map triple2text . concat $ r

toTriples :: [DocAsTriple ] -> [Triple]
-- the format requested by LitText
toTriples   = concat . map  makeRDFnt

newtype NLPtriple postag = NLPtriple Triple
    deriving (Eq, Ord, Show, Read)
--    deriving newtype Zeros
-- not feasible, probably because it is parametrized?

unNLPtriple (NLPtriple t) = t

class MakeIRI p where
-- make an IRI from a nRelID
    constructIRI ::  RDFsubj -> p -> RDFsubj

constructIRI_ :: Text -> RDFsubj -> [Text] -> RDFsubj
-- the internal code
-- the snip/doc SnipRelID is empty
constructIRI_ note rbase ts = if null ts
        then rbase
        else append2IRIwithSlash rbase
                (fromJustNote ("intercalate constructIRI  " ++ (t2s note)
                            ++ (concat . map show $ ts))
                            $ intercalate' "/" . reverse $ ts)
--            where base = toIRI rbase

instance MakeIRI SnipRelID where
    constructIRI rbase (SnipRelID ts)
            =   constructIRI_ "SnipRelID" rbase ts

instance MakeIRI SentenceRelID where
    constructIRI rbase (SentenceRelID ts)
            =  constructIRI_ "SentenceRelID" rbase ts

instance MakeIRI TokenRelID where
    constructIRI rbase (TokenRelID ts)
            =   constructIRI_ "TokenRelID" rbase ts

data DocAsTriple   =
    TriType {triSubj::RDFsubj , ty ::NLPtype}
    | TriPartOf {triSubj::RDFsubj , o :: RDFsubj }
--    | TriTextL  {triSubj::RDFsubj , p::NLPproperty, tl ::LCtext}
 -- should be language coded
    | TriTextL2  {triSubj::RDFsubj , pp::RDFproperty, tl ::LCtext}
     -- should be language coded
--    | TriText   {triSubj::RDFsubj , p::NLPproperty, te ::Ttext}
 -- should not be language coded
    | TriText2   {triSubj::RDFsubj , pp::RDFproperty, te ::Text}
    -- should not be language coded
--    | TriRel   {triSubj::RDFsubj , p::NLPproperty, o ::RDFsubj}
    | TriRel2   {triSubj::RDFsubj , pp::RDFproperty, o ::RDFsubj}
--    | TriList {triSubj::RDFsubj, p::NLPproperty, os :: [Text]}
    | TriList2 {triSubj::RDFsubj, pp::RDFproperty, os :: [Text]}
    | TriInt2 {triSubj::RDFsubj, pp::RDFproperty, int :: Int}

    | TriZero {}

    deriving (Show, Read, Eq, Ord, Generic)

instance Zeros (DocAsTriple  ) where zero = TriZero

makeTriple :: (Show postag) =>  postag -> RDFsubj
                    -> DocAsList postag -> [DocAsTriple ]

makeTriple _ base DocAsList {..} = [TriType (constructIRI base d3id)  Voc.Snip]

makeTriple _ base SentenceLin{..} = [TriType triSubj Voc.Sentence
                   , maybe zero (TriText2 triSubj
                   (mkRDFproperty Voc.SentenceParse)) s3parse
                   , TriPartOf triSubj $ constructIRI base s3docid
                   , TriTextL2 triSubj (mkRDFproperty SentenceForm) s3text]
                               -- sentence form not in the data
    where triSubj = constructIRI base s3id

-- | this gives all triples of a chain with the the same subj
makeTriple _ base DependenceLin{..} = [TriRel2 triSubj (mkRDFproperty d3type)
            $ constructIRI base d3depid
--            ,TriType triSubj Dependence
--            , TriText2 triSubj (mkRDFproperty DepOrigin) d3orig
--            , TriRel2 triSubj (mkRDFproperty DepGovernor) (constructIRI base d3govid)
--            , TriRel2 triSubj (mkRDFproperty DepDependent) (constructIRI base d3depid)
--            , TriText2 triSubj (mkRDFproperty DepGovGloss) d3govGloss
--            , TriText2 triSubj (mkRDFproperty DepDepGloss) d3depGloss
--            , TriPartOf triSubj $ constructIRI base d3sentence
                        ]
        -- uses the correct nlp prefix because d3type is a DepType
        -- how to find the places where the original type is not parsed?
        -- find earlier ??
    where   triSubj = constructIRI base d3govid

makeTriple _ base MentionLin{..} =
                [TriRel2 triSubj (mkRDFproperty Voc.Mentions)
                $ constructIRI base ment3Ment
--                        , TriType triSubj Mention
                    -- the triSubj is the same for all chains
                    -- most of this is not really necessary
--      , TriText2 triSubj (mkRDFproperty MentionRepresentative) (showT ment3Rep)
                                            -- is Bool
--      , TriRel2 triSubj (mkRDFproperty MentionSentence) $ constructIRI base ment3Sent
--      , TriRel2 triSubj (mkRDFproperty MentionStart) $ constructIRI base ment3Start
--      , TriRel2 triSubj (mkRDFproperty MentionStart) $ constructIRI base ment3End
--      , TriRel2 triSubj (mkRDFproperty MentionHead) $ constructIRI base ment3Head
            , TriTextL2 triSubj (mkRDFproperty MentionText) ment3Text
            , TriText2 triSubj (mkRDFproperty MentionType) ment3Type
            , TriText2 triSubj (mkRDFproperty MentionNumber) ment3Number
            , TriText2 triSubj (mkRDFproperty MentionGender) ment3Gender
            , TriText2 triSubj (mkRDFproperty MentionAnimacy) ment3Animacy
                        ]
        --  triSubj is a refers to o (mentions o
    where   triSubj = constructIRI base ment3Head

makeTriple _ base TokenLin{..} = [TriType
 triSubj Voc.Token
    ,  TriTextL2 triSubj  (mkRDFproperty Voc.WordForm) (word0 t3word)
    , TriTextL2 triSubj (mkRDFproperty Voc.Lemma3) (lemma0 t3lemma)
--            , TriInt2 triSubj (mkRDFproperty TokenBegin) t3begin  -- not used?
--            , TriInt2 triSubj (mkRDFproperty TokenEnd) t3end  -- not used?
    , TriText2 triSubj (mkRDFproperty Voc.Pos) (showT t3pos)
    -- gives the parsed form, not the original (Comma, not ,)
    -- better for queries with SPARQL
--        , TriText2 triSubj (mkRDFproperty TokenPosTT) (t3postt)
    , TriList2 triSubj (mkRDFproperty Voc.Ner) (map showT t3ner)
--                       (if t3ner == [NERunk "0"] then [] else map showT t3ner)
    , TriList2 triSubj (mkRDFproperty TokenSpeaker)(map fromSpeakerTag t3speaker)
   , TriPartOf triSubj $ constructIRI base t3sentenceID
    , TriList2 triSubj (mkRDFproperty TokenNERorig)
        (maybe [] id t3nerOrig)
    , (TriList2 triSubj (mkRDFproperty TokenPOSorig))
        $ maybe [] singleton t3posOrig
                               ]

    where
        triSubj = constructIRI base t3id
--        mbs = TriList2 triSubj (mkRDFproperty TokenNERorig)
--                (maybe [] id t3nerOrig)
--            ++ [fmap (TriText2 triSubj (mkRDFproperty TokenPOSorig)) t3posOrig
--            ]
--                :: [Maybe DocAsTriple]

makeTriple _ base NerLin{..} = [TriType triSubj NERentity
        , TriTextL2 triSubj (mkRDFproperty NerText) ner5text
        , TriRel2 triSubj (mkRDFproperty NerTokenEnd) $ constructIRI base ner5docTokenEnd
        , TriRel2 triSubj (mkRDFproperty NerTokenBegin2)$ constructIRI base  ner5docTokenEnd
        , TriRel2 triSubj (mkRDFproperty NerTokenEnd2) $ constructIRI base ner5docTokenEnd
        , TriText2 triSubj (mkRDFproperty NerType) (fromNERtag ner5ner)
            ]
    where
            triSubj = constructIRI base ner5docTokenBegin

makeRDFnt :: DocAsTriple -> [Triple]
-- | convert to a real RDF triple
makeRDFnt TriTextL2{..} =  singleton $ mkTripleLang3 (llang tl) (triSubj) pp (ltxt tl)
makeRDFnt TriText2{..} =  singleton $ mkTripleText  (triSubj) pp te
makeRDFnt TriRel2 {..} = singleton $ mkTripleRef (triSubj) pp o
makeRDFnt TriList2{..} = map (mkTripleText triSubj pp) os
makeRDFnt TriPartOf {..} = singleton $ mkTriplePartOf triSubj o
makeRDFnt TriType {..} = singleton $ mkTripleType triSubj (mkRDFtype ty)
makeRDFnt TriInt2 {..} = singleton $ mkTripleInt triSubj pp int
makeRDFnt TriZero = []
--makeRDFnt x = errorT ["makeRDFnt missing", showT x]
-- should use lang coded text
              --
singleton a = [a]
