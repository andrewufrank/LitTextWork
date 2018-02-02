{-----------------------------------------------------------------------------
--
-- Module      :  Dependency and other Codes
--
-- | use what NLP exports for Conll
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE        MultiParamTypeClasses
       , ScopedTypeVariables
        , FlexibleContexts
    , OverloadedStrings
        , TypeSynonymInstances
        , FlexibleInstances
        , DeriveGeneric
        #-}

module CoreNLP.POScodesConll (module CoreNLP.POScodesConll
        ,       NLPtypes.Tag(..)
--        , PosTagConll (..)
--        , Conll.Tag(..)
           ) where

import GHC.Generics
import Data.Serialize (Serialize)
import Test.Framework

import Uniform.Zero
import Uniform.Strings
import Uniform.Error
import qualified Data.Text as T   -- for the code copied from haskell-conll
import Text.Read (readEither)

--import qualified    NLP.Corpora.Conll  as Conll
import qualified NLP.Types.Tags as NLPtypes
import  NLP.Types.Tags (Tag(..))

--import NLP.Corpora.Conll (Tag(..))  -- unqualified

--import      NLP.Corpora.Conll                   -- from chatter package
--import      NLP.Corpora.Conll   as Conll
--import              NLP.Corpora.Conll  hiding (NERTag (..))
--import qualified    NLP.Types.Tags      as NLP (Tag(..))
--import qualified    NLP.Corpora.Conll   as NLP (Tag(..))
--import Safe

--instance Zeros POStagConll where zero = Unk
--type Unk = Conll.Unk

--type POStagConll = Conll.Tag   -- TODO


isVerbCode :: POStagConll -> Bool
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


coarsePOS :: POStagConll -> POStagConll         -- code muss hier sein, weil sonst Tag nicht importiert
coarsePOS p | isVerbCode p = VB
            | isNounCode p = NN
            | isAdjective p = JJ
            | isAdverb p = RB
            | isPunctuation p = Term  --
            | otherwise = p

isPOSpunctuation p = p >= Hash && p <= Colon

-----------------------------copied from NLP.Corpora.Conll
-- to avoid teh clash with Tag as type and class

-- | These tags may actually be the Penn Treebank tags.  But I have
-- not (yet?) seen the punctuation tags added to the Penn set.
--
-- This particular list was complied from the union of:
--
--   * All tags used on the Conll2000 training corpus. (contributing the punctuation tags)
--   * The PennTreebank tags, listed here: <https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html> (which contributed LS over the items in the corpus).
--   * The tags: START, END, and Unk, which are used by Chatter.
--

data POStagConll = START -- ^ START tag, used in training.
         | END -- ^ END tag, used in training.
         | Hash -- ^ #
         | Dollar -- ^ $
         | CloseDQuote -- ^ ''
         | OpenDQuote -- ^ ``
         | Op_Paren -- ^ (
         | Cl_Paren -- ^ )
         | Comma -- ^ ,
         | Term -- ^ . Sentence Terminator
         | Colon -- ^ :
         | CC -- ^ Coordinating conjunction
         | CD -- ^ Cardinal number
         | DT -- ^ Determiner
         | EX -- ^ Existential there
         | FW -- ^ Foreign word
         | IN -- ^ Preposition or subordinating conjunction
         | JJ -- ^ Adjective
         | JJR -- ^ Adjective, comparative
         | JJS -- ^ Adjective, superlative
         | LS -- ^ List item marker
         | MD -- ^ Modal
         | NN -- ^ Noun, singular or mass
         | NNS -- ^ Noun, plural
         | NNP -- ^ Proper noun, singular
         | NNPS -- ^ Proper noun, plural
         | PDT -- ^ Predeterminer
         | POS -- ^ Possessive ending
         | PRP -- ^ Personal pronoun
         | PRPdollar -- ^ Possessive pronoun
         | RB -- ^ Adverb
         | RBR -- ^ Adverb, comparative
         | RBS -- ^ Adverb, superlative
         | RP -- ^ Particle
         | SYM -- ^ Symbol
         | TO -- ^ to
         | UH -- ^ Interjection
         | VB -- ^ Verb, base form
         | VBD -- ^ Verb, past tense
         | VBG -- ^ Verb, gerund or present participle
         | VBN -- ^ Verb, past participle
         | VBP -- ^ Verb, non-3rd person singular present
         | VBZ -- ^ Verb, 3rd person singular present
         | WDT -- ^ Wh-determiner
         | WP -- ^ Wh-pronoun
         | WPdollar -- ^ Possessive wh-pronoun
         | WRB -- ^ Wh-adverb
         | Unk
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag POStagConll where
  fromTag = showTag

  parseTag txt = case readTag txt of
                   Left  _ -> Unk
                   Right t -> t

  -- | Constant tag for "unknown"
  tagUNK = Unk

  tagTerm = showTag

  startTag = START
  endTag = END

  isDt tag = tag `elem` [DT]

instance Arbitrary POStagConll where
  arbitrary = elements [minBound ..]
instance Serialize POStagConll

readTag :: Text -> ErrOrVal POStagConll
readTag "#" = Right Hash
readTag "$" = Right Dollar
readTag "(" = Right Op_Paren
readTag ")" = Right Cl_Paren
readTag "''" = Right CloseDQuote
readTag "``" = Right OpenDQuote
readTag "," = Right Comma
readTag "." = Right Term
readTag ":" = Right Colon
readTag txt =
  let normalized = replaceAll tagTxtPatterns (toUpper' txt)
  in  (readOrErr  normalized)

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "dollar")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: POStagConll -> Text
showTag Hash = "#"
showTag Op_Paren = "("
showTag Cl_Paren = ")"
showTag CloseDQuote = "''"
showTag OpenDQuote = "``"
showTag Dollar = "$"
showTag Comma = ","
showTag Term = "."
showTag Colon = ":"
showTag tag = replaceAll reversePatterns (T.pack $ show tag)

replaceAll :: [(Text, Text)] -> (Text -> Text)
replaceAll patterns = foldl (.) id (map (uncurry T.replace) patterns)





instance CharChains2 POStagConll String where
    show' =  show
instance CharChains2 POStagConll Text where
    show' =  s2t . show

instance Zeros POStagConll where zero = tagUNK
--type Unk = Conll.Unk



