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

module CoreNLP.DEPcodes (module CoreNLP.DEPcodes



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



data DepCode1 = ACL
                | ADVCL
                | ADVMOD
                | AMOD
                | APPOS
                | AUX
                | AUXPASS
                | CASE
                | CC  -- was CC but gives conflict with Conll.Tag
                | CCOMP
                | COMPOUND
                | CONJ
                | COP
                | CSUBJ
                | CSUBJPASS
                | DEP
                | DET
                | DISCOURSE
                | DISLOCATED
                | DOBJ
                | EXPL
                | FOREIGN
                | GOESWITH
                | IOBJ
                | LIST
                | MARK
                | MWE
                | NAME
                | NEG
                | NMOD
                | NSUBJ
                | NSUBJPASS
                | NUMMOD
                | PARATAXIS
                | PUNCT
                | REF   -- ??
                | REMNANT
                | REPARANDUM
                | ROOT
                | VOCATIVE
                | XCOMP
                | DepUnk

                deriving (Show, Read, Eq, Ord, Enum)
instance CharChains2 DepCode1 Text where
    show' = s2t . show

data DepCode2 = RELCL
            | AS  -- is this all prepositions?
            | ON
            | INTO
            | ABOUT
            | UPON
            | BUT
            | OF
            | IN  -- was IN, but conflicts with Conll.Tag
            | OVER
            | DOWN
            | AFTER
            | BEHIND
            | BY
            | WITH
            | LIKE
            | THAN
            | AGAINST
            | OR
            | DURING
            | FOR
            | TO  -- was TO, but conflicts with Conll.Tag
            | AT
            | AND
            | AGENT
            | POSS
            | TMOD
            | NPMOD
            | PRT
            | PREDET
            | TOWARDS
            | Missing String
            | DepZero

    deriving (Show, Read, Eq, Ord )
instance CharChains2 DepCode2 Text where
    show' = s2t . show


data DepCode = DepCode {d1::DepCode1
                        , d2 :: DepCode2
                        } deriving (Show, Read, Eq, Ord)
instance CharChains2 DepCode Text where
    show' = s2t . show

instance NiceStrings DepCode where
        shownice dc = (showT . d1 $ dc) <+> (showT . d2 $ dc)

isROOT = (ROOT==) . d1
isPUNCT = (PUNCT==) . d1

hasDepCode:: DepCode1 -> DepCode2 -> DepCode -> Bool
-- | test if d has major or minor depcode
hasDepCode maj min d = d1 d == maj && d2 d == min

readDepCodes :: Text -> DepCode
readDepCodes s = DepCode (readNoteTs ["parseDepCodes1 " , s
                , "split in", showT d12, "x2", showT x2]
                             .  toUpper' $ x1)
                            x2
    where
--        (d1, d2) = T.break (':'==) s
        d12 = splitOn' (":") s :: Maybe [Text]
        ds = fromJustNoteT ["readDepCodes in DependencyCodes"] d12
        (x1, x2) = case length ds of
            0 -> errorT ["readDepCodes - no code", showT s]
            1 -> (headNote "readDepCodes 1" ds, DepZero)
            2 -> (headNote "readDepCodes 1" ds
                    , readDef (Missing d2') d2')
        d2 = headNote "readDepCodes head 2" . tailNote "readDepCodes tail" $ ds

        d2' = t2s . toUpper' $ d2
--        dd1 = readNote "parseDepCodes1 " . t2s .  toUpper' $ d1
--        dd2 = if null' d2 then DepZero else dd3
--        dd3 = readDef (Missing d2') d2'  :: DepCode2
----        dd3 = maybe (error  d2') id (maybeRead d2')  :: DepCode2

test_depCode_cc = assertEqual (DepCode CC DepZero) (readDepCodes "cc")

isSimpleCode ::  DepCode1 -> DepCode -> Bool
isSimpleCode aa d = d1 d == aa

makeDepCode :: DepCode1 -> DepCode2 -> DepCode
makeDepCode maj min = DepCode maj min

makeSimpleDepCode :: DepCode1 -> DepCode
makeSimpleDepCode maj = DepCode maj zero

showDepCodes :: DepCode -> Text
showDepCodes (DepCode dd1 dd2)  = if dd2==DepZero then show' dd1
        else (concat' [show' dd1, ":", show' dd2])

--instance Zeros DepCode where zero = (z)
instance Zeros DepCode1 where zero =  DepUnk
instance Zeros DepCode2 where zero =  DepZero
