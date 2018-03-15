-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- linearize doc11 and convert to triples
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
-- Linearize Doc11

data DocAsList postag = DocAsList {d3id:: DocRelID}
    | SentenceLin { s3id :: SentenceRelID
                    , s3parse :: Maybe Text  -- the parse tree
                    , s3text :: Text -- the sentence text combined from the tokens
                }
    | DependenceLin {d3type :: DepCode -- Text -- String
--                        , d3orig :: Text -- the value given in the XML
                        , d3govid :: TokenRelID
                        , d3depid :: TokenRelID
--                        , d3govGloss :: Text
--                        , d3depGloss :: Text
                        }
    | MentionLin {
--            ment3Rep ::  Bool -- , indicates the representative mention
--        , mentSent :: SentenceID
          ment3Ment :: TokenRelID  -- missing?? find in chain
        , ment3Start, ment3End :: TokenRelID -- not used ??
        , ment3Head :: TokenRelID  -- the head of the mention
--        , ment3Text :: Text  -- multiple words, the actual mention - not yet used
        }
    | TokenLin { t3id :: TokenRelID
                    , t3word :: Wordform0
                    , t3lemma :: Lemma0
--                    , t3begin, t3end :: Int  -- not used
                    , t3pos :: postag --  the pos tag recognized
--                    , t3posOrig :: Text -- the pos tag received
--                    , t3postt :: Text -- the pos from the tree tagger
                    , t3ner :: [NERtag] -- [Text] -- String
                    , t3speaker :: [SpeakerTag] -- Text -- String
                    }
      | ZeroLin {}

        deriving (Show, Read, Eq, Ord, Generic)
instance Zeros (DocAsList postag) where zero = ZeroLin

class Linearize d postag where
    linearize :: postag -> d -> [DocAsList postag]

instance Linearize (Doc11 postag) postag where
    linearize ph Doc11{..} = DocAsList {..}
        : (sents ++ cos)
     where
        d3id = doc11id
        sents = concat $ map (linearize ph) doc11sents:: [DocAsList postag]
        cos = maybe [] (linearize ph) doc11corefs :: [DocAsList postag]

instance Linearize (Sentence11 postag) postag where
    linearize ph Sentence11{..} = SentenceLin {s3parse = s11parse
                                                , s3id = s11id
                                                , s3text = t1
                                                }
                : (concat $ map (linearize ph) s11toks
                    ++ maybe [] (map (linearize ph)) s11deps
                )
        where
            t1 = T.replace "  " " " . concat' . map getTokenText $ s11toks
            -- replace double blanks by a single one for the sentence
            getTokenText :: Token11 postag -> Text
            getTokenText Token11{..} = concat'
                . catMaybes $ [t11before, Just . word0 $ t11word, t11after]


instance Linearize (Token11 postag) postag where
    linearize ph Token11 {..} = [TokenLin {..}]
        where
        t3id = t11id
        t3word =  t11word
        t3lemma =  t11lemma
        t3pos =   t11pos
        t3posOrig = t11posOrig
        t3postt = t11postt
        t3ner =  t11ner -- when is this a list?
                        -- use the Ner2 values?
        t3speaker =  t11speaker

instance Linearize  Coreferences11 postag where
    linearize ph Coreferences11{..} = map (linearizeMention zero) mc
        where
            cs = co11chains :: [MentionChain11]


            mc = concat $ map  mentions cs :: [Mention11]
            linearizeMention rep Mention11 {..} = MentionLin {..}
                where
                        ment3Ment = rep
--                        ment3Rep = ment11Rep
            --            ment3Sent = ment11Sent
                        ment3Start = ment11Start
                        ment3End = ment11End  -- points next word
                        ment3Head = ment11Head
                        ment3Text = ment11Text

--processOneMentionChain :: MentionChain11 -> (TokenRelID, MentionChain11)
---- find the rep
--processOneMentionChain mc =

instance Linearize Dependence11 postag where
    linearize ph Dependence11 {..} =  [DependenceLin{..}]
        where
            d3type = d11type
--            d3orig = d11orig
            d3govid =d11govid
            d3depid = d11depid
--            d3govGloss = d11govGloss
--            d3depGloss = d11depGloss



            --


                --
