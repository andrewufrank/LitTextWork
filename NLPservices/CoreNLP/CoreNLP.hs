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


--import GHC.Generics
--import qualified NLP.Tags      as NLP
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
--import Data.Maybe
--import Data.List
--import qualified NLP.TagSets.Conll  as Conll
--import qualified NLP.TagSets.UD as UD

json2NT :: PartURI -> Text -> NTtext
-- | main operation, convert JSON text file to a triple (NT) text file
json2NT rdfbase = toNT . toTriple rdfbase . toLin . to11op . to1op .  decodeDoc2op
