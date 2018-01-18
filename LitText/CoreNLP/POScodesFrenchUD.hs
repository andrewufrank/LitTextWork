{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for French parser for French
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
-- pos tageset name is
-- from http://www.llf.cnrs.fr/Gens/Abeille/French-Treebank-fr.php
-- model is http://nlp.stanford.edu/software/stanford-french-corenlp-2017-06-09-models.jar
-- called with -serverProperties StanfordCoreNLP-french-UD.properties
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

module CoreNLP.POScodesFrenchUD (module CoreNLP.POScodesFrenchUD
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
import  NLP.Types.Tags as NLPtypes (Tag(..))
--import      NLP.Corpora.Conll
--import      NLP.Corpora.Conll   as Conll

--type POStagEng = Conll.Tag   -- renames the ConllTag
--instance CharChains2 POStagEng Text

data POStagFrenchUD =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
--    Dollar | -- ^ $
--    Comma  | -- ^ ,
--    Point | -- ^ .
--    OpenBracket |   -- [
    Dollarpoint | --    $.       |   --	0
    Dollaropenbracket | --  $[       |   --	 '
    Dollarcomma  |   --	,
    ADJA       |   --	environs
    ADJD       |   --	I.
    ADP |
    ADV       |   --	que
    APPO       |   --	l'épouse
    APPR       |   --	 --
    APPRART       |   --	 --
    APZR       |   --	avoir
    ART       |   --	DES
    CARD       |   --	XI
    FM       |   --	tous
    ITJ       |   --	oui
    KON       |   --	un
    KOUS       |   --	sous
    NE       |   --	XXII
    NOUN |
    NN       |   --	CONCLUSION
    PDAT       |   --	d'analyse
    PDS       |   --	une
    PIAT       |   --	ajouta
    PIDAT       |   --	jeune
    PIS       |   --	aller
    PPER       |   --	du
    PPOSAT       |   --	donner
    PRELS       |   --	qui
    PRF       |   --	café
    PRON |
    PROAV       |   --	d'un
    PTKANT       |   --	avec
    PTKNEG       |   --	net
    PTKVZ       |   --	fort
    PWAV       |   --	dit
    PWS       |   --	mon
    TRUNC       |   --	en
    VAFIN       |   --	C'est
    VAINF       |   --	sein
    VMFIN       |   --	démêlés
    VERB |
    VVFIN       |   --	chrétienne
    VVIMP       |   --	j'
    VVINF       |   --	bien
    VVIZU       |   --	hésitation
    VVPP       |   --	maintenant
    X |
    XY       |   --	n
    Frenchunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag POStagFrenchUD where
--parseTag :: Text -> POStag
    parseTag txt = case readTag txt of
                   Left  _ -> NLPtypes.tagUNK
                   Right t -> t

    tagUNK = Frenchunk

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` []  -- unknown what is a det here?

instance Arbitrary POStagFrenchUD where
  arbitrary = elements [minBound ..]
instance Serialize POStagFrenchUD

readTag :: Text -> ErrOrVal POStagFrenchUD
--readTag "#" = Right Hash
--readTag "$" = Right Dollar
--readTag "(" = Right Op_Paren
--readTag ")" = Right Cl_Paren
--readTag "''" = Right CloseDQuote
--readTag "``" = Right OpenDQuote
--readTag "," = Right Comma
--readTag "." = Right Point
--readTag "." = Right Term
--readTag ":" = Right Colon
--readTag "[" = Right Openbracket

readTag txt =
  let normalized = replaceAll tagTxtPatterns (toUpper' txt)
  in  (readOrErr  normalized)

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "Dollar")    -- because dollar is always in first position, capitalize
                                        -- better solution is probably to use toUpper
                                        -- and define DOLLARPOINT etc.
                   , ("[", "openbracket")
                   , (",", "comma")
                   , (".", "point")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: POStagFrenchUD -> Text
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

--readTag :: Text -> ErrOrVal POStagFrenchUD
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag POStagFrenchUD 34232"
--maybe2errorP (Just a) = Right a

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

instance CharChains2 POStagFrenchUD String where
    show' =  show
instance CharChains2 POStagFrenchUD Text where
    show' =  s2t . show

instance Zeros POStagFrenchUD where zero = NLPtypes.tagUNK
--type Unk = Conll.Unk

test_french_tag1 :: IO ()
test_french_tag1 = assertEqual (Dollaropenbracket::POStagFrenchUD) (parseTag "$["::POStagFrenchUD)
test_french_tag2 :: IO ()
test_french_tag2 = assertEqual (Dollarpoint::POStagFrenchUD) (parseTag "$."::POStagFrenchUD)
test_french_tag3 :: IO ()
test_french_tag3 = assertEqual (Dollarcomma::POStagFrenchUD) (parseTag "$,"::POStagFrenchUD)
test_french_tag4 :: IO ()
test_french_tag4 = assertEqual (VVINF::POStagFrenchUD) (parseTag "VVINF"::POStagFrenchUD)

test_french_tagR :: IO ()
test_french_tagR = assertEqual ("Dollaropenbracket"::Text) (replaceAll tagTxtPatterns (toUpper'   "$[")::Text)
