{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric
, ScopedTypeVariables #-}
-- | Data types representing the tagsets used in a model

module CoreNLP.Models (
    module CoreNLP.Models
    ) where


import  NLP.Types.Tags  (  POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
import Data.Utilities

import qualified NLP.Corpora.Brown as B
import qualified NLP.Corpora.German as German
import qualified NLP.Corpora.Conll as C
import qualified NLP.Corpora.ConllNER as C
--import  NLP.Corpora.Brown (POStag )

data Notag
notag  = undefined :: Notag

data NLPmodel pos ner dep = NLPmodel
            { modelURL :: Text
            , posTags :: pos
            , nerTags :: ner
            , depTags :: dep
            } deriving (Show, Read, Ord, Eq)

brownModel = NLPmodel "Brown" (undefined :: B.POStag) notag notag
germanModel = NLPmodel "http://nlp.stanford.edu/software/stanford-german-corenlp-2017-06-09-models.jar"
    (undefined :: German.POStag) (undefined::C.NERtag) notag
englishModel = NLPmodel "http://nlp.stanford.edu/software/stanford-english-corenlp-2017-06-09-models.jar"
        (undefined :: C.POStag) (undefined::C.NERtag) notag
