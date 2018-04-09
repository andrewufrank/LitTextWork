-----------------------------------------------------------------------------
--
-- Module      :  LitText.Foundation . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances
    , DeriveGeneric
    , RecordWildCards
    , DeriveAnyClass #-}

module LitText.CoreNLP  (
            SpeakerTags (..)
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
            , SpeakerTag (..), NERtag (..)
            , NTtext (..), unNT
            , module LitText.CoreNLP.Vocabulary
            , json2triples, conllu2triples
--            , Snip, SnipSigl, buchURIx
        )  where

import LitText.CoreNLP.CoreNLP
import LitText.CoreNLP.DocNLP_0or1
import LitText.CoreNLP.Doc1_absoluteID
import LitText.CoreNLP.Doc2ToLinear
    --  toLin ::  postag ->  (Doc11 postag) ->  [DocAsList postag]
import LitText.CoreNLP.Linear2Triple
import LitText.CoreNLP.Conllu2doc1
import LitText.CoreNLP.Vocabulary
--import Data.List
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD
import LitText.Foundation (Snip)

