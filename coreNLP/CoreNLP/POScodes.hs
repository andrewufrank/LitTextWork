{-----------------------------------------------------------------------------
--
-- Module      :  Dependency and other Codes
--
-- |
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveAnyClass
        #-}

module CoreNLP.POScodes (module CoreNLP.POScodes
        , module NLP.Corpora.Conll
--
--        DepCode1(..), DepCode2 (..), DepCode
--        , isROOT, isPUNCT
----        , hasDepCode
--        , makeSimpleDepCode, makeDepCode
--        , Pos (..)  -- , Unk
--        , NERTag (..)
--        , isVerbCode, isNounCode, isPunctuation, isAdjective
--        , isClosedClass
--        , isSimpleCode
--        , isPOSpunctuation
--        , coarsePOS
--        , readDepCodes, showDepCodes
--        , SpeakerTag (..), readSpeakerTag
--        , Conll.Tag (..)

        )
         where

import           Test.Framework

import Uniform.Zero
import Uniform.Strings
import Uniform.Error

--import qualified    NLP.Corpora.Conll  as Conll
import      NLP.Corpora.Conll
import      NLP.Corpora.Conll   as Conll
--import              NLP.Corpora.Conll  hiding (NERTag (..))
--import qualified    NLP.Types.Tags      as NLP (Tag(..))
--import qualified    NLP.Corpora.Conll   as NLP (Tag(..))
--import Safe

instance Zeros Pos where zero = Unk
--type Unk = Conll.Unk


isVerbCode :: Tag -> Bool
isVerbCode v = v `elem` [VB, VBD, VBG, VBN, VBZ, VBP]
--         | VB -- ^ Verb, base form
--         | VBD -- ^ Verb, past tense
--         | VBG -- ^ Verb, gerund or present participle
--         | VBN -- ^ Verb, past participle
--         | VBP -- ^ Verb, non-3rd person singular present
--         | VBZ -- ^ Verb, 3rd person singular present

isNounCode v = v `elem` [NN, NNS, NNP, NNPS]
--            NN -- ^ Noun, singular or mass
--         | NNS -- ^ Noun, plural
--         | NNP -- ^ Proper noun, singular
--         | NNPS -- ^ Proper noun, plural
isPunctuation v = v `elem` [CloseDQuote -- ^ ''
         , OpenDQuote -- ``
         , Op_Paren -- ^ (
         , Cl_Paren -- ^ )
         , Comma -- ^ ,
         ,  Colon -- ^ :
         , Term -- ^ .
         ]
isAdjective v = v `elem` [JJ, JJR, JJS]
isAdverb v = v `elem` [RB, RBR, RBS]
isClosedClass v = not (isNounCode v || isVerbCode v ||  isAdjective v
            || isPunctuation v)


coarsePOS :: Tag -> Tag         -- code muss hier sein, weil sonst Tag nicht importiert
coarsePOS p | isVerbCode p = VB
            | isNounCode p = NN
            | isAdjective p = JJ
            | isAdverb p = RB
            | isPunctuation p = Term  --
            | otherwise = p

type Pos  = Conll.Tag

isPOSpunctuation p = p >= Conll.Hash && p <= Conll.Colon

instance CharChains2 Pos Text where
    show' = s2t . show


