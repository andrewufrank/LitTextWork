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

module CoreNLP.CoreNLP (
        module CoreNLP.CoreNLP
        ,  SpeakerTags (..)
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
            , SpeakerTag (..), NERtag (..)
            , NTtext (..), unNT
        )  where

--import              Uniform.Strings
--import Uniform.Zero
--import   NLP.TagSets.Conll hiding (NERtag (..))
--import              NLP.TagSets.DEPcodes
--import              NLP.TagSets.NERcodes
--import              NLP.TagSets.SpeakerTags
import CoreNLP.DocNLP_0or1
import CoreNLP.Doc1_absoluteID
import CoreNLP.Doc2ToLinear
import CoreNLP.Linear2Triple
import CoreNLP.Conllu2doc1

--import GHC.Generics
--import qualified NLP.Tags      as NLP
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
--import Data.Maybe
--import Data.List
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD

json2NT :: (UD.POStags postag) =>
        postag -> LanguageCode -> PartURI -> Text -> NTtext
-- | main operation, convert JSON text file to a triple (NT) text file
json2NT postag lang rdfbase = doc1toNT postag rdfbase
                . to1op postag lang .  decodeDoc2op

conllu2NT rdfbase = doc1toNT UD.undefPOS rdfbase . conllu2doc1

doc1toNT :: (Show postag, UD.POStags postag) =>
            postag -> PartURI -> Doc1 postag -> NTtext
doc1toNT postag rdfbase = toNT . toTriple postag rdfbase
            . toLin postag . to11op postag