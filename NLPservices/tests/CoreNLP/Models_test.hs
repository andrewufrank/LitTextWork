{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric
, ScopedTypeVariables #-}
-- | Data types representing the tagsets used in a model

module NLP.Models  where


import  NLP.Types.Tags  (NERtags (..), POStags (..), TagsetIDs (..)
                    , ChunkTags (..))
import Data.Utilities

import qualified NLP.Corpora.Brown as B
import qualified NLP.Corpora.German as German
import qualified NLP.Corpora.Conll as C
import qualified NLP.Corpora.ConllNER as C
--import  NLP.Corpora.Brown (POStag )

import NLP.Models

test_models = assertEqual 1 1
