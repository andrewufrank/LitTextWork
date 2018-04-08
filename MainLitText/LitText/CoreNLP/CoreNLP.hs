-----------------------------------------------------------------------------
--
-- Module      :  the top exporting all contents of CoreNLP

-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
        , FlexibleInstances
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , RecordWildCards
    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LitText.CoreNLP.CoreNLP (
        module LitText.CoreNLP.CoreNLP
        ,  SpeakerTags (..)
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
            , SpeakerTag (..), NERtag (..)
            , NTtext (..), unNT
        )  where
import LitText.CoreNLP.DocNLP_0or1
import LitText.CoreNLP.Doc1_absoluteID
import LitText.CoreNLP.Doc2ToLinear
    --  toLin ::  postag ->  (Doc11 postag) ->  [DocAsList postag]
import LitText.CoreNLP.Linear2Triple
import LitText.CoreNLP.Conllu2doc1
--import Data.List
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD

json2NT :: (UD.POStags postag) =>
        postag -> LanguageCode -> RDFsubj -> Text -> NTtext
-- | main operation, convert JSON text file to a triple (NT) text file
json2NT postag lang rdfbase = doc1toNT postag rdfbase
                . to1op postag lang .  decodeDoc2op

json2triples:: (UD.POStags postag) =>
        postag -> LanguageCode -> RDFsubj -> Text -> [Triple]
-- | main operation, convert JSON text file to a triple (NT) text file
json2triples postag lang rdfbase = doc1toTriples postag rdfbase
                . to1op postag lang .  decodeDoc2op

conllu2NT rdfbase = doc1toNT UD.undefPOS rdfbase . conllu2doc1
conllu2triples rdfbase = doc1toTriples UD.undefPOS rdfbase . conllu2doc1

doc1toNT :: (Show postag, UD.POStags postag) =>
            postag -> RDFsubj -> Doc1 postag -> NTtext
doc1toNT postag rdfbase = toNT . toTriple postag rdfbase
            . toLin postag . to11op postag

doc1toTriples :: (Show postag, UD.POStags postag) =>
            postag -> RDFsubj -> Doc1 postag -> [Triple]
doc1toTriples postag rdfbase = toTriples . toTriple postag rdfbase
            . toLin postag . to11op postag
