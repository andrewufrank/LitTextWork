{-----------------------------------------------------------------------------
--
-- Module       | --  Dependency and other Codes
--
-- | the codes for French parser for French
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

module CoreNLP.POScodesFrench (module CoreNLP.POScodesFrench
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

--type POStagEng = Conll.Tag   -- renames the ConllTag
--instance CharChains2 POStagEng Text

data POStagFrench =   -- copied from http://universaldependencies.org/u/pos/
    START  | -- START tag, used in training.
    END | --END tag, used in training.
    Dollar | -- ^ $
    Comma  | -- ^ ,
    Point | -- ^ .
    OpenBracket |   -- [
    Dollarpoint | --    $. 	    |   --	0
    DollaropenBracket | --  $[ 	    |   --	 '
    Dollarcomma 	    |   --	,
    ADJA 	    |   --	environs
    ADJD 	    |   --	I.
    ADV 	    |   --	que
    APPO 	    |   --	l'épouse
    APPR 	    |   --	 --
    APPRART 	    |   --	 --
    APZR 	    |   --	avoir
    ART 	    |   --	DES
    CARD 	    |   --	XI
    FM 	    |   --	tous
    ITJ 	    |   --	oui
    KON 	    |   --	un
    KOUS 	    |   --	sous
    NE 	    |   --	XXII
    NN 	    |   --	CONCLUSION
    PDAT 	    |   --	d'analyse
    PDS 	    |   --	une
    PIAT 	    |   --	ajouta
    PIDAT 	    |   --	jeune
    PIS 	    |   --	aller
    PPER 	    |   --	du
    PPOSAT 	    |   --	donner
    PRELS 	    |   --	qui
    PRF 	    |   --	café
    PROAV 	    |   --	d'un
    PTKANT 	    |   --	avec
    PTKNEG 	    |   --	net
    PTKVZ 	    |   --	fort
    PWAV 	    |   --	dit
    PWS 	    |   --	mon
    TRUNC 	    |   --	en
    VAFIN 	    |   --	C'est
    VAINF 	    |   --	sein
    VMFIN 	    |   --	démêlés
    VVFIN 	    |   --	chrétienne
    VVIMP 	    |   --	j'
    VVINF 	    |   --	bien
    VVIZU 	    |   --	hésitation
    VVPP 	    |   --	maintenant
    XY 	    |   --	n
    Frenchunk  -- other  -- conflicts possible!
        deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance NLPtypes.Tag POStagFrench where
--parseTag :: Text -> POStag
    parseTag txt = case readTag txt of
                   Left  _ -> NLPtypes.tagUNK
                   Right t -> t

    tagUNK = Frenchunk

    tagTerm = showTag

    startTag = START
    endTag = END

    isDt tag = tag `elem` []  -- unknown what is a det here?

instance Arbitrary POStagFrench where
  arbitrary = elements [minBound ..]
instance Serialize POStagFrench

readTag :: Text -> ErrOrVal POStagFrench
--readTag "#" = Right Hash
readTag "$" = Right Dollar
--readTag "(" = Right Op_Paren
--readTag ")" = Right Cl_Paren
--readTag "''" = Right CloseDQuote
--readTag "``" = Right OpenDQuote
readTag "," = Right Comma
readTag "." = Right Point
--readTag "." = Right Term
--readTag ":" = Right Colon
readTag "[" = Right OpenBracket

readTag txt =
  let normalized = replaceAll tagTxtPatterns (toUpper' txt)
  in  (readOrErr  normalized)

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "dollar")
                   , ("[", "bracket")
                   , (",", "comma")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: POStagFrench -> Text
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

--readTag :: Text -> ErrOrVal POStagFrench
--readTag txt = maybe2errorP . read . t2s $ txt
--
--maybe2errorP  :: Maybe a -> ErrOrVal a
--maybe2errorP Nothing = Left "readTag POStagFrench 34232"
--maybe2errorP (Just a) = Right a

readOrErr :: Read a => Text -> Either Text a
readOrErr    t = case (readEither (t2s t)) of
                        Left msg -> Left (s2t msg)
                        Right a -> Right a

instance CharChains2 POStagFrench String where
    show' =  show
instance CharChains2 POStagFrench Text where
    show' =  s2t . show

instance Zeros POStagFrench where zero = NLPtypes.tagUNK
--type Unk = Conll.Unk



