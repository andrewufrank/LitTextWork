-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- linearize doc4 and convert to triples  in .lin5
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module CoreNLP.Doc2ToLinear
    ( module CoreNLP.Doc2ToLinear
    , module CoreNLP.Doc1_absoluteID
    , Doc11 (..)
    ) where

import           Uniform.Strings
import CoreNLP.Doc1_absoluteID
import Uniform.Zero
import Data.Maybe
import GHC.Generics
import qualified Data.Text as T   -- replace
--import LitTypes.LanguageTypedText  (unLCtext, LCtext (..), LanguageCodedText (..) )
-- should be imported
import qualified NLP.TagSets.Conll  as Conll
import qualified NLP.TagSets.UD as UD


toLin ::  postag ->  (Doc11 postag) ->  [DocAsList postag] -- the entry point
toLin  postag  =  linearize postag ()

--toLinUD ::   (Doc11 UD.POStag) ->  [DocAsList UD.POStag] -- the entry point
--toLinUD   =  linearize UD.undefUPOS ()

data DocAsList postag = DocAsList {d3id:: SnipIRelD}
    | SentenceLin { s3id :: SentenceRelID
                    , s3parse :: Maybe Text  -- the parse tree
                    , s3text :: LCtext -- the sentence text combined from the tokens
                    , s3docid :: SnipIRelD  -- for the part of
                }
    | DependenceLin {d3type :: DepCode -- Text -- String
                        , d3orig :: Text -- the value given in the XML
                        , d3govid :: TokenRelID
                        , d3depid :: TokenRelID
                        , d3govGloss :: LCtext
                        , d3depGloss :: LCtext
                    , d3sentence :: SentenceRelID -- for partOf
                        }
    | MentionLin {
            ment3Rep ::  Bool -- , indicates the representative mention
        , ment3Sent :: SentenceRelID
         , ment3Ment :: TokenRelID  -- missing?? find in chain
        , ment3Start, ment3End :: TokenRelID -- not used ??
        , ment3Head :: TokenRelID  -- the head of the mention
        , ment3Text :: LCtext   -- multiple words, the actual mention - not yet used
        , ment3Type, ment3Number, ment3Gender, ment3Animacy :: Text
        }
    | TokenLin { t3id :: TokenRelID
                    , t3word :: Wordform0
                    , t3lemma :: Lemma0
                    , t3begin, t3end :: Int  -- not used
                    , t3pos :: postag --  the pos tag recognized
                    , t3posOrig :: Maybe Text -- the pos tag received
--                    , t3postt :: Text -- the pos from the tree tagger
                    , t3ner :: [NERtagExt] -- [Text] -- String
                    , t3nerOrig :: Maybe [Text]
                    , t3speaker :: [SpeakerTag] -- Text -- String
                    , t3sentence :: SentenceRelID -- for partOf
                    }
                        -- | the record from the s_entitymentions
    | NerLin {ner5docTokenBegin :: TokenRelID
                , ner5docTokenEnd :: TokenRelID
                , ner5tokenBegin :: TokenRelID
                , ner5tokenEnd :: TokenRelID
                , ner5text :: LCtext
                , ner5characterOffsetBegin :: Int
                , ner5characterOffsetEnd :: Int
                , ner5ner :: NERtag -- the code ??
                }
      | ZeroLin {}

        deriving (Show, Read, Eq, Ord, Generic)
instance Zeros (DocAsList postag) where zero = ZeroLin

class Linearize d postag partOf where
    linearize :: postag -> partOf -> d -> [DocAsList postag]

instance Linearize (Doc11 postag) postag () where
    linearize ph _ Doc11{..} = DocAsList {..}
        : (sents ++ cos)
     where
        d3id = doc11id
        sents = concat $ map (linearize ph doc11id) doc11sents:: [DocAsList postag]
        cos = maybe [] (linearize ph doc11id) doc11corefs :: [DocAsList postag]

instance Linearize (Sentence11 postag) postag SnipIRelD where
    linearize ph pa Sentence11{..} = SentenceLin {s3parse = s11parse
                                                , s3id = s11id
                                                , s3text = t2
                                                , s3docid = pa
                                                }
                : (concat $ map (linearize ph s11id) s11toks
                    ++ maybe [] (map (linearize ph s11id)) s11deps
                    ++ maybe [] (map (linearize ph s11id)) s11ner

                )
        where
            t2 = LCtext t1 lang
            t1 = trim' . T.replace "  " " " . concat' . map getTokenText $ s11toks
            -- replace double blanks by a single one for the sentence
            -- remove blanks front and back -
            getTokenText :: Token11 postag -> Text
            getTokenText Token11{..} = concat'
                . catMaybes $ [t11before, Just  . getText . word0 $ t11word, t11after]
            getLanguage Token11{..} = getLanguageCode . word0 $ t11word
            lang = if null s11toks then NoLanguage
                            else getLanguage . headNote "linearize sentence 11" $ s11toks

instance Linearize (Token11 postag) postag SentenceRelID where
    linearize ph pa Token11 {..} = [TokenLin {..}]
        where
        t3id = t11id
        t3word =  t11word
        t3lemma =  t11lemma
        t3pos =   t11pos
        t3posOrig = t11posOrig
--        t3postt = t11postt
        t3ner =  t11ner -- when is this a list?
                        -- use the Ner2 values?
        t3nerOrig = t11nerOrig
        t3speaker =  t11speaker
        t3before = t11before
        t3after = t11after
        t3begin = t11begin
        t3end = t11end
        t3sentence = pa

instance Linearize  Coreferences11 postag SnipIRelD where
    linearize _ _  Coreferences11{..} = map (linearizeMention) mc
        where
            cs = co11chains :: [MentionChain11]


            mc = concat $ map  mentions cs :: [Mention11]
            linearizeMention Mention11 {..} = MentionLin {..}
                where
                        ment3Ment = ment11Referent
                        ment3Rep = ment11Rep
                        ment3Sent = ment11Sent
                        ment3Start = ment11Start
                        ment3End = ment11End  -- points next word
                        ment3Head = ment11Head
                        ment3Text = ment11Text
                        ment3Type = ment11Type
                        ment3Number = ment11Number
                        ment3Gender = ment11Gender
                        ment3Animacy = ment11Animacy


instance Linearize Dependence11 postag SentenceRelID where
    linearize _ pa Dependence11 {..} =  [DependenceLin{..}]
        where
            d3type = d11type
            d3orig = d11orig
            d3govid =d11govid
            d3depid = d11depid
            d3govGloss = d11govGloss
            d3depGloss = d11depGloss
            d3sentence = pa

instance Linearize Ner4 postag SentenceRelID  where
    linearize _ pa Ner4{..} = [NerLin{..}]
        where
            ner5docTokenBegin = ner4docTokenBegin
            ner5docTokenEnd =   ner4docTokenEnd
            ner5tokenBegin =   ner4tokenBegin
            ner5tokenEnd =   ner4tokenEnd
            ner5text =   ner4text
            ner5characterOffsetBegin = ner4characterOffsetBegin
            ner5characterOffsetEnd = ner4characterOffsetEnd
            ner5ner = ner4ner


            --


                --
