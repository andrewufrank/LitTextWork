{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the tagsets used in a model

module CoreNLP.Models_test  where


--import  NLP.Types.Tags  (NERtags (..), POStags (..), TagsetIDs (..)
--                    , ChunkTags (..))
--import Data.Utilities
import              Test.Framework
import              Uniform.TestHarness

--import qualified NLP.Corpora.Brown as B
--import qualified NLP.Corpora.German as German
--import qualified NLP.Corpora.Conll as C
--import qualified NLP.Corpora.ConllNER as C
--import  NLP.Corpora.Brown (POStag )

import CoreNLP.Models

test_models = assertEqual 1 1
