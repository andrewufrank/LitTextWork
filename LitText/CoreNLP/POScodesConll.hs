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
        , DeriveAnyClass
        #-}

module CoreNLP.POScodesConll (module CoreNLP.POScodesConll
--        , module NLP.Corpora.Conll
        , Conll.Tag(..)
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
import qualified Data.Text as T   -- for the code copied from haskell-conll
import Text.Read (readEither)

import qualified    NLP.Corpora.Conll  as Conll
import qualified NLP.Types.Tags as NLPtypes
import NLP.Corpora.Conll (Tag(..))  -- unqualified

--import      NLP.Corpora.Conll                   -- from chatter package
--import      NLP.Corpora.Conll   as Conll
--import              NLP.Corpora.Conll  hiding (NERTag (..))
--import qualified    NLP.Types.Tags      as NLP (Tag(..))
--import qualified    NLP.Corpora.Conll   as NLP (Tag(..))
--import Safe

instance Zeros PosTagConll where zero = Unk
--type Unk = Conll.Unk

type PosTagConll = Conll.Tag   -- TODO


isVerbCode :: PosTagConll -> Bool
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


coarsePOS :: PosTagConll -> PosTagConll         -- code muss hier sein, weil sonst Tag nicht importiert
coarsePOS p | isVerbCode p = VB
            | isNounCode p = NN
            | isAdjective p = JJ
            | isAdverb p = RB
            | isPunctuation p = Term  --
            | otherwise = p

--data PosTag = START -- ^ START tag, used in training.
--         | END -- ^ END tag, used in training.
--         | Hash -- ^ #
--         | Dollar -- ^ $
--         | CloseDQuote -- ^ ''
--         | OpenDQuote -- ^ ``
--         | Op_Paren -- ^ (
--         | Cl_Paren -- ^ )
--         | Comma -- ^ ,
--         | Term -- ^ . Sentence Terminator
--         | Colon -- ^ :
--         | CC -- ^ Coordinating conjunction
--         | CD -- ^ Cardinal number
--         | DT -- ^ Determiner
--         | EX -- ^ Existential there
--         | FW -- ^ Foreign word
--         | IN -- ^ Preposition or subordinating conjunction
--         | JJ -- ^ Adjective
--         | JJR -- ^ Adjective, comparative
--         | JJS -- ^ Adjective, superlative
--         | LS -- ^ List item marker
--         | MD -- ^ Modal
--         | NN -- ^ Noun, singular or mass
--         | NNS -- ^ Noun, plural
--         | NNP -- ^ Proper noun, singular
--         | NNPS -- ^ Proper noun, plural
--         | PDT -- ^ Predeterminer
--         | POS -- ^ Possessive ending
--         | PRP -- ^ Personal pronoun
--         | PRPdollar -- ^ Possessive pronoun
--         | RB -- ^ Adverb
--         | RBR -- ^ Adverb, comparative
--         | RBS -- ^ Adverb, superlative
--         | RP -- ^ Particle
--         | SYM -- ^ Symbol
--         | TO -- ^ to
--         | UH -- ^ Interjection
--         | VB -- ^ Verb, base form
--         | VBD -- ^ Verb, past tense
--         | VBG -- ^ Verb, gerund or present participle
--         | VBN -- ^ Verb, past participle
--         | VBP -- ^ Verb, non-3rd person singular present
--         | VBZ -- ^ Verb, 3rd person singular present
--         | WDT -- ^ Wh-determiner
--         | WP -- ^ Wh-pronoun
--         | WPdollar -- ^ Possessive wh-pronoun
--         | WRB -- ^ Wh-adverb
--         | Unk Text
--  deriving (Read, Show, Ord, Eq)  -- ,  Bounded, Generic, Enum,

isPOSpunctuation p = p >= Hash && p <= Colon

instance CharChains2 PosTagConll Text where
    show' = s2t . show

---- the following code is copied from haskell-conll
--
--readTag :: Text -> ErrOrVal Tag
--readTag "#" = Right Hash
--readTag "$" = Right Dollar
--readTag "(" = Right Op_Paren
--readTag ")" = Right Cl_Paren
--readTag "''" = Right CloseDQuote
--readTag "``" = Right OpenDQuote
--readTag "," = Right Comma
--readTag "." = Right Term
--readTag ":" = Right Colon
--readTag txt =
--  let normalized = replaceAll tagTxtPatterns (T.toUpper txt)
--  in  eitherString2Text (readEither $ T.unpack normalized)
--
---- | Order matters here: The patterns are replaced in reverse order
---- when generating tags, and in top-to-bottom when generating tags.
--tagTxtPatterns :: [(Text, Text)]
--tagTxtPatterns = [ ("$", "dollar")
--                 ]
--replaceAll :: [(Text, Text)] -> (Text -> Text)
--replaceAll patterns = foldl (.) id (map (uncurry T.replace) patterns)
--
--eitherString2Text :: Either String Tag -> ErrOrVal Tag
--eitherString2Text (Left a) = Left (s2t a)
--eitherString2Text (Right t) = Right t
