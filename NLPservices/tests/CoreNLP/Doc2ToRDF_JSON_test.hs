-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module CoreNLP.Doc2ToRDF_JSON_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import CoreNLP.Doc2ToRDF_JSON

--import           Uniform.Strings
--import Uniform.FileIO
----import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.UTF8 as B
--import Data.Aeson (eitherDecode)
--
--import CoreNLP.ParseJsonCoreNLP
--import CoreNLP.Doc2ToRDF_JSON
--import qualified NLP.Corpora.Conll  as Conll
----import Text.Show.Pretty (valToStr)
----import Text.PrettyPrint.GenericPretty
--import Data.Aeson.Encode.Pretty
--import Data.Aeson
--import GHC.Exts



--instance ShowTestHarness Doc2 where
--instance ShowTestHarness a => ShowTestHarness (ErrOrVal a) where
--
--instance (Zeros a) => Zeros (ErrOrVal a) where zero = Right zero


