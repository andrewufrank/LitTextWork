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
module CoreNLP.ParseJsonCoreNLP_test
     where


import           Test.Framework
import Uniform.Test.TestHarness
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)
import qualified NLP.Corpora.Conll  as Conll

import CoreNLP.ParseJsonCoreNLP

import Data.Aeson.Encode.Pretty
import Data.Aeson
import GHC.Exts

test_toJson = assertEqual objVBG (toJSON (Conll.VBG))

objVBG = String "VBG"

test_short1 = do   -- this test work relative to working dir, not test dir
    res0   <- runErr $ do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String Doc2
        putIOwords ["decoded:", showT r]
        writeFile2 (makeRelFile "short1.doc2x") (showT r)

    assertBool True


decodeDoc2op :: Text ->   Doc2
decodeDoc2op f = either (const zero) id r
    where
        r = eitherDecode flbs :: Either String Doc2
        flbs = b2bl . t2b $ f :: LazyByteString

progName = "nlpservices"
test_A = testFile2File progName "short1.json" "short1.doc2" decodeDoc2op

instance ShowTestHarness Doc2 where
instance ShowTestHarness a => ShowTestHarness (ErrOrVal a) where

instance (Zeros a) => Zeros (ErrOrVal a) where zero = Right zero

restest = zero :: Doc2

instance Zeros Doc2 where zero = Doc2 zero Nothing

showStartJson = s2t . take 100 . B.toString

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a



