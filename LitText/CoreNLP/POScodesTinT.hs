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

data PosTagTinT =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
--    ADJ | -- adjective
--    ADP | -- adposition
--    ADV | -- adverb
--    AUX | -- auxiliary
--    CCONJ | -- coordinating conjunction
    DET | -- determiner
--    INTJ | -- interjection
--    NOUN | -- noun
--    NUM | -- numeral
--    PART | -- particle
--    PRON | -- pronoun
--    PROPN | -- proper noun
--    PUNCT | -- punctuation
--    SCONJ | -- subordinating conjunction
--    SYM | -- symbol
--    VERB | -- verb
    TinTunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag PosTagTinT where
--parseTag :: Text -> PosTag
    parseTag txt = case readTag txt of
                   Left  _ -> NLPtypes.tagUNK
                   Right t -> t

    tagUNK = TinTunk

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` [DET]

instance Arbitrary PosTagTinT where
  arbitrary = elements [minBound ..]
instance Serialize PosTagTinT

readTag :: Text -> ErrOrVal PosTagTinT
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
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: PosTagTinT -> Text
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

--readTag :: Text -> ErrOrVal PosTagTinT
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag PosTagTinT 34232"
--maybe2errorP (Just a) = Right a

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

instance CharChains2 PosTagTinT String where
    show' =  show
instance CharChains2 PosTagTinT Text where
    show' =  s2t . show

instance Zeros PosTagTinT where zero = NLPtypes.tagUNK
--type Unk = Conll.Unk



