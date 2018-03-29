-----------------------------------------------------------------------------
--
-- Module      :  Doc1_absoluteID (relative to DocID
--
-- | the base data types for the Doc11
-- Doc11 has all RelIDs relative to the SentenceID
-- the postag is coded here
-- convertes .doc3 to .doc4 (doc11) files
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
import   NLP.TagSets.Conll hiding (NERtag (..))
import              NLP.TagSets.DEPcodes
import              NLP.TagSets.NERcodes
import              NLP.TagSets.SpeakerTags
import CoreNLP.DocNLP_0or1
import GHC.Generics
import qualified NLP.Tags      as NLP
--import CoreNLP.ParseJsonCoreNLP -- the doc2 and ...
import Data.Maybe
import Data.List
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD

to11op ::   (POStags postag) => postag -> (Doc1 postag) ->  (Doc11 postag)  -- the entry point
to11op postag =  convertToAbsoluteID postag  (SnipRelID ["doc11"])

to11opUD ::   (Doc1 UD.POStag) ->  (Doc11 UD.POStag)  -- the entry point
to11opUD  =  convertToAbsoluteID UD.undefPOS  (SnipRelID ["doc11"])

class ConvertToAbsulteID postag relID a2 a1 where
-- convert to the 1 or 0 records
    convertToAbsoluteID :: postag -> relID -> a2 -> a1

data Doc11 postag = Doc11 {doc11sents:: [Sentence11 postag]
                 , doc11corefs :: Maybe Coreferences11   -- only one
                 , doc11id :: SnipRelID     } deriving (Show, Read, Eq, Ord, Generic)


instance Zeros (Doc11 postag) where zero = Doc11 [] zero zero

data Sentence11 postag = Sentence11 {s11id :: SentenceRelID
                        , s11parse :: Maybe Text  -- the parse tree
                        , s11toks :: [Token11 postag]
                        , s11deps :: Maybe [Dependence11]
                        , s11ner :: Maybe [Ner4]
                        -- should be only one or none
                        }  deriving (Show, Read, Eq, Ord, Generic)


data Dependence11 = Dependence11 {d11type :: DepCode -- Text -- String
                        , d11orig :: Text -- the value given in the XML
                        , d11govid :: TokenRelID
                        , d11depid :: TokenRelID
                        , d11govGloss :: LCtext
                        , d11depGloss :: LCtext
                        } deriving (Show, Read, Eq, Ord, Generic)

data Coreferences11 = Coreferences11 {co11chains:: [MentionChain11]}
                deriving (Show, Read, Eq, Ord, Generic)

data MentionChain11 = MentionChain11 {mentions:: [Mention11]}
        deriving (Show, Read, Eq, Ord, Generic)

data Mention11 = Mention11 {ment11Rep ::  Bool -- , indicates the representative mention
        , ment11Sent :: SentenceRelID
        , ment11Start, ment11End :: TokenRelID -- not used ??
        , ment11Head :: TokenRelID  -- the head of the mention
        , ment11Text :: LCtext  -- multiple words, the actual mention - not yet used
        , ment11Referent :: TokenRelID -- the head of the referent
                -- what the text mentions in the original
                -- the referent is kept, recognized by rep == true and
                -- ment11Head == ment11Referent
        , ment11Type, ment11Number, ment11Gender, ment11Animacy :: Text
        }
  deriving (Show, Read, Eq, Ord, Generic)

data Token11 postag = Token11 { t11id :: TokenRelID
                    , t11word :: Wordform0
                    , t11lemma :: Lemma0
                    , t11begin, t11end :: Int  -- not used
                    , t11pos :: postag --  the pos tag recognized
                    , t11posOrig :: Maybe Text -- the pos tag received
--                    , t11postt :: Text -- the pos from the tree tagger
                    , t11ner :: [NERtagExt] -- [Text] -- String
                    , t11nerOrig :: Maybe [Text]
                    , t11speaker :: [SpeakerTag] -- Text -- String
                    , t11before, t11after :: Maybe Text
                    }   deriving (Show, Read, Eq, Ord, Generic)

-- | the record from the s_entitymentions
data Ner4 = Ner4 {ner4docTokenBegin :: TokenRelID
                , ner4docTokenEnd :: TokenRelID
                , ner4tokenBegin :: TokenRelID
                , ner4tokenEnd :: TokenRelID
                , ner4text :: LCtext
                , ner4characterOffsetBegin :: Int
                , ner4characterOffsetEnd :: Int
                , ner4ner :: NERtag -- the code ??
                }
        deriving (Show, Read, Eq, Ord, Generic)


instance (NLP.POStags postag)
        => ConvertToAbsulteID postag SnipRelID (Doc1 postag) (Doc11 postag) where
    convertToAbsoluteID posPh d@(SnipRelID _) Doc1{..} = Doc11 {..}
      where
        doc11sents = map (convertToAbsoluteID posPh doc11id) doc1sents
        doc11corefs =  fmap (convertToAbsoluteID posPh doc11id) doc1corefs
        doc11id = d

instance (NLP.POStags postag)
    => ConvertToAbsulteID postag SnipRelID (Sentence1 postag) (Sentence11 postag) where

    convertToAbsoluteID  posPh d@(SnipRelID _) Sentence1 {..} = Sentence11 {..}
        where
            s11id = addSent2DocID d s1id
            s11parse = s1parse
            s11toks = map (convertToAbsoluteID posPh s11id)  s1toks
            s11deps = fmap (map (convertToAbsoluteID posPh s11id)) s1deps
            s11ner = fmap (map (convertToAbsoluteID posPh s11id)) s1entitymentions

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
--        t11postt = zero
        t11ner =  tner -- when is this a list?
                        -- use the Ner2 values?
        t11nerOrig =  tnerOrig
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

instance ConvertToAbsulteID postag SnipRelID Coreferences1 Coreferences11 where
    convertToAbsoluteID phP s Coreferences1{..} = Coreferences11{..}
        where
            co11chains = map (convertToAbsoluteID phP s) coChains

instance ConvertToAbsulteID postag SnipRelID MentionChain1 MentionChain11 where
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

instance ConvertToAbsulteID postag SnipRelID (Mention1) Mention11 where
    convertToAbsoluteID _  s (Mention1 {..}) = Mention11 {..}
        where
            ment11Rep = mentRep
            ment11Sent = addSent2DocID s mentSent
            ment11Start = addTok2SentID ment11Sent mentStart
            ment11End = addTok2SentID ment11Sent mentEnd  -- points next word
            ment11Head = addTok2SentID ment11Sent mentHead
            ment11Text = mentText
            ment11Referent = undefined "undefined Referent asdwqer"
            ment11Type = mentType
            ment11Number = mentNumber
            ment11Gender = mentGender
            ment11Animacy = mentAnimacy

markMentionsWithRep ::   TokenRelID -> Mention11 -> Mention11
markMentionsWithRep rep ment  = ment  {ment11Referent = rep}

instance ConvertToAbsulteID postag SentenceRelID Ner3 Ner4 where
    convertToAbsoluteID _ s  Ner3{..} = Ner4 {..}
        where
            ner4docTokenBegin = addTok2SentID s ner3docTokenBegin
            ner4docTokenEnd = addTok2SentID s ner3docTokenEnd
            ner4tokenBegin = addTok2SentID s ner3tokenBegin
            ner4tokenEnd = addTok2SentID s ner3tokenEnd
            ner4text =   ner3text
            ner4characterOffsetBegin = ner3characterOffsetBegin
            ner4characterOffsetEnd = ner3characterOffsetEnd
            ner4ner = ner3ner

