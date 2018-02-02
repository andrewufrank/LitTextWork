{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for UD  -- the table is lifted
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
--        , DeriveAnyClass
        , DeriveGeneric
        #-}

module CoreNLP.POScodesUD (module CoreNLP.POScodesUD
--        , module NLP.Corpora.Conll
        , NLPtypes.POStags(..)
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

data POStagUD =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    ADJ | -- adjective
    ADP | -- adposition
    ADV | -- adverb
    AUX | -- auxiliary
    CCONJ | -- coordinating conjunction
    DET | -- determiner
    INTJ | -- interjection
    NOUN | -- noun
    NUM | -- numeral
    PART | -- particle
    PRON | -- pronoun
    PROPN | -- proper noun
    PUNCT | -- punctuation
    SCONJ | -- subordinating conjunction
    SYM | -- symbol
    VERB | -- verb
    X  -- other
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.POStags POStagUD where
--parseTag :: Text -> PosTag
    parseTag txt = case readTag txt of
                   Left  _ -> NLPtypes.tagUNK
                   Right t -> t

    tagUNK = X

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` [DET]

instance Arbitrary POStagUD where
  arbitrary = elements [minBound ..]
instance Serialize POStagUD

readTag :: Text -> ErrOrVal POStagUD
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

showTag :: POStagUD -> Text
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

--readTag :: Text -> ErrOrVal POStagUD
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag POStagUD 34232"
--maybe2errorP (Just a) = Right a

-- @since 4.6.0.0
readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

instance CharChains2 POStagUD String where
    show' =  show
instance CharChains2 POStagUD Text where
    show' =  s2t . show

instance Zeros POStagUD where zero = X
--type Unk = Conll.Unk



