{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for TinT parser for italian
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
        , DeriveGeneric
        #-}

module CoreNLP.POScodesTinT (module CoreNLP.POScodesTinT
--        , module NLP.Corpora.Conll
--        , ErrOrVal (..)
        )
         where

import GHC.Generics
import Data.Serialize (Serialize)

import           Test.Framework

import Uniform.Zero
import Uniform.Strings
import Uniform.Error
import Data.Text   as T (replace)
import Text.Read (readEither)
--import qualified NLP.Corpora.Conll      as Conll

import qualified NLP.Types.Tags as NLPtypes
--import      NLP.Corpora.Conll
--import      NLP.Corpora.Conll   as Conll

--type PosTagEng = Conll.Tag   -- renames the ConllTag
--instance CharChains2 PosTagEng Text

data POStagTinT =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    A         | --	felice
    AP         | --	nostro
    B         | --	domani
    BplusPC         | --	eccolo
    BN         | --	non
    CC         | --	e
    CS         | --	che
    DD         | --	quel
    DE         | --	che
    DI         | --	ogni
    DQ         | --	che
    DR         | --	cui
    E         | --	a
    EplusRD         | --	dalla
    FB         | --	 -
    FC         | --	 ;
    FF         | --	 -
    FS         | --	 ?
    I         | --	Oh
    N         | --	Sei
    NO         | --	ultima
    PC         | --	ti
    PCplusPC         | --	gliele
    PD         | --	quello
    PE         | --	Noi
    PI         | --	tutto
    PP         | --	mio
    PQ         | --	Che
    PR         | --	Che
    RD         | --	il   -- RD?
    RI         | --	una
    S         | --	nutrice
    SP         | --	FULVIA
    SW         | --	grand'
    T         | --	tutti
    V         | --	vedere
    VplusPC         | --	avervi
    VplusPCplusPC         | --	occuparsene
    VA         | --	 è
    VAplusPC         | --	averlo
    VM         | --	volevo
    VMplusPC         | --	poterci
    VMplusPCplusPC         | --	sferrarsene
    X          --	FINE
--    TinTunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag POStagTinT where
--parseTag :: Text -> PosTag
    parseTag txt = case readTag txt of
                   Left  _ -> NLPtypes.tagUNK
                   Right t -> t

    tagUNK = X

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` [RD]

instance Arbitrary POStagTinT where
  arbitrary = elements [minBound ..]
instance Serialize POStagTinT

readTag :: Text -> ErrOrVal POStagTinT
--readTag "#" = Right Hash
--readTag "$" = Right Dollar
--readTag "(" = Right Op_Paren
--readTag ")" = Right Cl_Paren
--readTag "''" = Right CloseDQuote
--readTag "``" = Right OpenDQuote
--readTag "," = Right Comma
--readTag "." = Right Term
--readTag ":" = Right Colon
readTag txt =
  let normalized = replaceAll tagTxtPatterns (toUpper' txt)
  in  (readOrErr  normalized)

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "dollar")
                   , ("+", "plus")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: POStagTinT -> Text
--showTag Hash = "#"
--showTag Op_Paren = "("
--showTag Cl_Paren = ")"
--showTag CloseDQuote = "''"
--showTag OpenDQuote = "``"
--showTag Dollar = "$"
--showTag Comma = ","
--showTag Term = "."
--showTag Colon = ":"
showTag tag = replaceAll reversePatterns (s2t $ show tag)

replaceAll :: [(Text, Text)] -> (Text -> Text)
replaceAll patterns = foldl (.) id (map (uncurry  T.replace) patterns)

--readTag :: Text -> ErrOrVal POStagTinT
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag POStagTinT 34232"
--maybe2errorP (Just a) = Right a

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

instance CharChains2 POStagTinT String where
    show' =  show
instance CharChains2 POStagTinT Text where
    show' =  s2t . show

instance Zeros POStagTinT where zero = NLPtypes.tagUNK
--type Unk = Conll.Unk



