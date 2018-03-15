-----------------------------------------------------------------------------
--
-- Module      :  Doc1_absoluteID (relative to DocID
--
-- | the base data types for the Doc11
-- Doc11 has all RelIDs relative to the SentenceID
-- the postag is coded here
-----------------------------------------------------------------------------
{-# LANGUAGE
        ScopedTypeVariables
        , FlexibleContexts
        , FlexibleInstances
    , OverloadedStrings
    ,Arrows
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , RecordWildCards
    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CoreNLP.Doc1_absoluteID (
        module CoreNLP.DocNLP_0or1
        , module CoreNLP.Doc1_absoluteID
        ,  SpeakerTags (..)
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
            , SpeakerTag (..), NERtag (..)
        )  where

import              Uniform.Strings
import Uniform.Zero
import   NLP.Corpora.Conll
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import CoreNLP.DocNLP_0or1
import GHC.Generics
import qualified NLP.Types.Tags      as NLP
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
import Data.Maybe
import Data.List

class ConvertToAbsulteID postag relID a2 a1 where
-- convert to the 1 or 0 records
    convertToAbsoluteID :: postag -> relID -> a2 -> a1

data Doc11 postag = Doc11 {doc11sents:: [Sentence11 postag]
                 , doc11corefs :: Maybe Coreferences11   -- only one
                 , doc11id :: DocRelID     } deriving (Show, Read, Eq, Ord, Generic)


instance Zeros (Doc11 postag) where zero = Doc11 [] zero zero

data Sentence11 postag = Sentence11 {s11id :: SentenceRelID
                        , s11parse :: Maybe Text  -- the parse tree
                        , s11toks :: [Token11 postag]
                        , s11deps :: Maybe [Dependence11]
                        -- should be only one or none
                        }  deriving (Show, Read, Eq, Ord, Generic)


data Dependence11 = Dependence11 {d11type :: DepCode -- Text -- String
--                        , d11orig :: Text -- the value given in the XML
                        , d11govid :: TokenRelID
                        , d11depid :: TokenRelID
--                        , d11govGloss :: Text
--                        , d11depGloss :: Text
                        } deriving (Show, Read, Eq, Ord, Generic)

data Coreferences11 = Coreferences11 {co11chains:: [MentionChain11]}
                deriving (Show, Read, Eq, Ord, Generic)

data MentionChain11 = MentionChain11 {mentions:: [Mention11]}
        deriving (Show, Read, Eq, Ord, Generic)

data Mention11 = Mention11 {ment11Rep ::  Bool -- , indicates the representative mention
--        , mentSent :: SentenceID
        , ment11Start, ment11End :: TokenRelID -- not used ??
        , ment11Head :: TokenRelID  -- the head of the mention
        , ment11Text :: Text  -- multiple words, the actual mention - not yet used
        , ment11Referent :: TokenRelID -- the head of the referent
                -- what the text mentions in the original
                -- the referent is kept, recognized by rep == true and
                -- ment11Head == ment11Referent
        }
  deriving (Show, Read, Eq, Ord, Generic)

data Token11 postag = Token11 { t11id :: TokenRelID
                    , t11word :: Wordform0
                    , t11lemma :: Lemma0
                    , t11begin, t11end :: Int  -- not used
                    , t11pos :: postag --  the pos tag recognized
                    , t11posOrig :: Text -- the pos tag received
                    , t11postt :: Text -- the pos from the tree tagger
                    , t11ner :: [NERtag] -- [Text] -- String
                    , t11speaker :: [SpeakerTag] -- Text -- String
                    , t11before, t11after :: Maybe Text
                    }   deriving (Show, Read, Eq, Ord, Generic)


instance (NLP.POStags postag)
        => ConvertToAbsulteID postag DocRelID (Doc1 postag) (Doc11 postag) where
    convertToAbsoluteID posPh d@(DocRelID _) Doc1{..} = Doc11 {..}
      where
        doc11sents = map (convertToAbsoluteID posPh doc11id) doc1sents
        doc11corefs =  fmap (convertToAbsoluteID posPh doc11id) doc1corefs
        doc11id = d

instance (NLP.POStags postag)
    => ConvertToAbsulteID postag DocRelID (Sentence1 postag) (Sentence11 postag) where

    convertToAbsoluteID  posPh d@(DocRelID _) Sentence1 {..} = Sentence11 {..}
        where
            s11id = addSent2DocID d s1id
            s11parse = s1parse
            s11toks = map (convertToAbsoluteID posPh s11id)  s1toks
            s11deps = fmap (map (convertToAbsoluteID posPh s11id)) s1deps

instance (NLP.POStags postag)
        => ConvertToAbsulteID postag SentenceRelID
                        (Token0 postag) (Token11 postag) where

    convertToAbsoluteID posPh s (Token0 {..}) = Token11 {..}
      where
        t11id = addTok2SentID s tid
        t11word =  tword
        t11lemma =  tlemma
        t11pos =   tpos
        t11posOrig = tposOrig
        t11postt = zero
        t11ner =  tner -- when is this a list?
                        -- use the Ner2 values?
        t11speaker =  tspeaker
        t11before = tbefore
        t11after = tafter
        t11begin = tbegin
        t11end = tend

instance ConvertToAbsulteID postag SentenceRelID Dependence1 Dependence11 where
    convertToAbsoluteID _  s Dependence1 {..} = Dependence11 {..}
        where
            d11type = d1type
            d11orig = d1orig
            d11govid = addTok2SentID s  d1govid
            d11depid = addTok2SentID s  d1depid
            d11govGloss = d1govGloss
            d11depGloss = d1depGloss

instance ConvertToAbsulteID postag DocRelID Coreferences1 Coreferences11 where
    convertToAbsoluteID phP s Coreferences1{..} = Coreferences11{..}
        where
            co11chains = map (convertToAbsoluteID phP s) coChains

instance ConvertToAbsulteID postag DocRelID MentionChain1 MentionChain11 where
    convertToAbsoluteID phP s (MentionChain1 mentions) = MentionChain11 $
            map (markMentionsWithRep (ment11Head rep')) mentions2

        -- phantom is not used
        where
            mentions2 = (map (convertToAbsoluteID phP s) mentions)
            (rep,norep) = partition ment11Rep mentions2
            rep' = case length rep of
                1 -> headNote "mkCorefTriple2 rep not present" rep :: Mention11
                _ -> errorT ["mkCoreTriple2 - mentions exist, but not a single true rep",
                        showT mentions, showT rep]

instance ConvertToAbsulteID postag DocRelID (Mention1) Mention11 where
    convertToAbsoluteID _  s (Mention1 {..}) = Mention11 {..}
        where
            ment11Rep = mentRep
            ment11Sent = addSent2DocID s mentSent
            ment11Start = addTok2SentID ment11Sent mentStart
            ment11End = addTok2SentID ment11Sent mentEnd  -- points next word
            ment11Head = addTok2SentID ment11Sent mentHead
            ment11Text = mentText
            ment11Referent = undefined "undefined Referent asdwqer"

markMentionsWithRep ::   TokenRelID -> Mention11 -> Mention11
markMentionsWithRep rep ment  = ment  {ment11Referent = rep}
--
