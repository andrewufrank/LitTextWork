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

module CoreNLP.NERcodes (module CoreNLP.NERcodes
--    DepCode1(..), DepCode2 (..), DepCode
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

import              NLP.Corpora.Conll  hiding (NERTag (..))


-- | Named entity categories defined for the Conll 2003 task.
data NERTag = PER
            | ORG
            | LOC
            | MISC
            | NERunk
            -- found in Stanford CoreNLP 3.5.2
            -- Time, Location, Organization, Person, Money, Percent, Date
            | O
            | NUMBER
            | PERSON
            | DURATION
            | DATE
            | SET
            | TIME
            | ORDINAL
            | LOCATION
            | ORGANIZATION
            | MONEY
            | PERCENT

  deriving (Read, Show, Ord, Eq,  Enum, Bounded)

instance Zeros NERTag where zero = NERunk

data SpeakerTag = PER0 | PER1 | PER2 | Speaker Text
    deriving (Read, Show,  Ord, Eq)
    -- to encode the speaker tag -- any others? PER5 or 5 is seen

-- readSpeakerTag :: Text -> SpeakerTag
-- readSpeakerTag = readNoteT "readSpeakerTag"

readSpeakerTag :: Text -> SpeakerTag
readSpeakerTag  t = case t of
                "PER0" -> PER0
                "PER1" -> PER1
                "PER2" -> PER2
                s     -> Speaker s

instance CharChains2 SpeakerTag Text  where
    show' PER0 = "PERO"
    show' PER1 = "PER1"
    show' PER2 = "PER2"
    show' (Speaker n) = "Speaker " <> showT n


