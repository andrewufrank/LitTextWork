-----------------------------------------------------------------------------
--
-- Module      :  Doc1_absoluteID (relative to DocID
--
-- | the base data types for the Doc11
-- Doc11 has all RelIDs relative to the SentenceID
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
--        , module CoreNLP.POScodes
        ,  SpeakerTags (..)
--        , module CoreNLP.NERcodes -- import separately when needed
            , DepCode (..), DEPtags (..) -- parseDEPtag, hasDepCode
            , DepCode1 (..), DepCode2 (..)
--        , module CoreNLP.DEPcodes  -- import separately when needed
        -- ,readDocString
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

class ConvertToAbsulteID postag relID a2 a1 where
-- convert to the 1 or 0 records
    convertToAbsoluteID :: postag -> relID -> a2 -> a1




data Doc11 postag = Doc11 {doc11Sents:: [Sentence11 postag]
                 , doc11Corefs :: Maybe Coreferences11   -- only one
                       } deriving (Show, Read, Eq, Ord, Generic, Zeros)

--instance Zeros (Doc1 postag) where zero = Doc1 [] zero

data Sentence11 postag = Sentence11 {s11id :: SentenceRelID
                        , s11parse :: Maybe Text  -- the parse tree
                        , s11toks :: [Token11 postag]
                        , s11deps :: Maybe [Dependence11]
                        -- should be only one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        }  deriving (Show, Read, Eq, Ord, Generic, Zeros)


data Dependence11 = Dependence11 {d11type :: DepCode -- Text -- String
                        , d11orig :: Text -- the value given in the XML
                        , d11govid :: TokenRelID
                        , d11depid :: TokenRelID
                        , d11govGloss :: Text
                        , d11depGloss :: Text
                        } deriving (Show, Read, Eq, Ord, Generic, Zeros)

data Coreferences11 = Coreferences11 {co11Chains:: [MentionChain11]}
                deriving (Show, Read, Eq, Ord, Generic, Zeros)

data MentionChain11 = MentionChain11 [Mention11]
        deriving (Show, Read, Eq, Ord, Generic, Zeros)

data Mention11 = Mention11 {ment11Rep ::  Bool -- , indicates the representative mention
--        , mentSent :: SentenceID
        , ment11Start, ment11End :: TokenRelID -- not used ??
        , ment11Head :: TokenRelID  -- the head of the mention
        , ment11Text :: Text  -- multiple words, the actual mention - not yet used
        }
  deriving (Show, Read, Eq, Ord, Generic, Zeros)

data Token11 postag = Token11 { t11id :: TokenRelID
                    , t11word :: Wordform0
                    , t11lemma :: Lemma0
--                    , t11begin, t11end :: Int  -- not used
                    , t11pos :: postag --  the pos tag recognized
                    , t11posOrig :: Text -- the pos tag received
                    , t11postt :: Text -- the pos from the tree tagger
                    , t11ner :: [NERtag] -- [Text] -- String
                    , t11speaker :: [SpeakerTag] -- Text -- String
                    }   deriving (Show, Read, Eq, Ord, Generic, Zeros)


instance (NLP.POStags postag)
        => ConvertToAbsulteID postag DocRelID (Doc1 postag) (Doc11 postag) where
    convertToAbsoluteID posPh d@(DocRelID _) Doc1{..} = Doc11 {..}
      where
        doc11Sents = map (convertToAbsoluteID posPh doc11id) doc1Sents
        doc11Corefs =  fmap (convertToAbsoluteID posPh doc11id) doc1Corefs
        doc11id = d
                -- chains of mentions


instance (NLP.POStags postag)
    => ConvertToAbsulteID postag DocRelID (Sentence1 postag) (Sentence11 postag) where

    convertToAbsoluteID  posPh d@(DocRelID _) Sentence1 {..} = Sentence11 {..}
        where
            s11id = addSent2DocID d s1id
            s11parse = s1parse
            s11toks = map (convertToAbsoluteID posPh s11id)  s1toks
            s11deps = fmap (map (convertToAbsoluteID posPh s11id)) s1deps
--                        s_enhancedPlusPlusDependencies of
--                    Just d3 -> Just $ map (convertToAbsoluteID posPh s11id) d3
--                    Nothing -> Nothing


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
--                    maybe [] (\a -> [a]) $ tspeaker
--        t11begin = tcharacterOffsetBegin
--        t11end = tcharacterOffsetEnd

instance ConvertToAbsulteID postag SentenceRelID Dependence1 (Dependence11) where
    convertToAbsoluteID _  s Dependence1 {..} = Dependence11 {..}
        where
            d11type = d1type
            d11orig = d1orig
            d11govid = addTok2SentID s  d1govid
            d11depid = addTok2SentID s  d1depid
            d11govGloss = d1govGloss
            d11depGloss = d1depGloss

instance ConvertToAbsulteID postag DocRelID Coreferences1 (Coreferences11) where
    convertToAbsoluteID phP s Coreferences1{..} = Coreferences11{..}
        where
            co11Chains = map (convertToAbsoluteID phP s) coChains

instance ConvertToAbsulteID postag DocRelID MentionChain1 MentionChain11 where
    convertToAbsoluteID phP s (MentionChain1 cs) =
            MentionChain11 (map (convertToAbsoluteID phP s) cs)
        -- phantom is not used

instance ConvertToAbsulteID postag DocRelID (Mention1) Mention11 where
    convertToAbsoluteID _  s (Mention1 {..}) = Mention11 {..}
        where
            ment11Rep = mentRep
            ment11Sent = addSent2DocID s mentSent
            ment11Start = addTok2SentID ment11Sent mentStart
            ment11End = addTok2SentID ment11Sent mentEnd  -- points next word
            ment11Head = addTok2SentID ment11Sent mentHead
            ment11Text = mentText

--
