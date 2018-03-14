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
module CoreNLP.ParseJsonCoreNLP_test  -- (openMain, htf_thisModuelsTests)
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

test_A = testFile2File "nlp/short1.json" "nlp/short1.doc2" decodeDoc2op

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

-- these test work relative to working dir, not test dir
--test_nlpjson  = do
--    res0   <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
--        let r = eitherDecode  f  :: Either String Doc2
--        putIOwords ["decoded:", showT r]
--        let r2 = either (error "test_nlpjson Left") id  r :: Doc2
--        file2 <- readFile2 (makeRelFile "short1.doc2")
--        let e2 = readNote "read test_nlpjson werwe" file2 :: Doc2
--        when (r2/= e2) $ writeFile2 (makeRelFile "short1.doc2x") (showT r2)
--        return (r2,e2)
--    let (r3,e3) = either (\m -> error ("test_nlpjson Left 2" ++ t2s m) ) id  res0
--    assertEqual  e3 r3

-- lazy bytestring and ghci/test have a memory leak
--test_nlpjson  = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
--        let r = eitherDecode  f  :: Either String Doc2
--        putIOwords ["decoded:", showT r]
--        r2 <- runErrorFromEither r
--        putIOwords ["expected res:", showT restest]
--        let r3 = r2 :: Doc2 -- either (error "sdfawer") id r2
--        let r2p = pretty r3
--        let restest_p = pretty restest
--        writeFile2 (makeRelFile "nlpjson.res") r2p
--        writeFile2 (makeRelFile "nlpjson.exp") restest_p
--
--        return (r2p, restest_p)
--
----    let res0p = bb2s . bl2b $ encodePretty res0
----    putIOwords ["pretty resp2", s2t res2p]
----    putIOwords ["pretty res0p", s2t res0p]
--    let (r2p, restest_p) = either (error "sdafwe") id res0
--    assertEqual r2p restest_p

--test_pretty = do
--    res0 :: ErrOrVal Doc2 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
--        let r = eitherDecode  f  :: Either String Doc2
--        runErrorFromEither r
--    case res0 of
--        Left msg -> runErr $ putIOwords ["left", msg]
--        Right res1 -> runErr $ do
--            putIOwords ["decoded:", showT res1]
--            let fn2 = makeRelFile "short1.doc2"
--            writeFile2 fn2 (showT res1)
--            let res1p = bl2b $ encodePretty res1  :: ByteString
--            putIOwords ["pretty", showT $ b2t res1p]
--            let fn3 = makeRelFile "short1.pretty"
----            writeFile2 fn3 (fromJustNote "x" . b2t $  res1p)
--            writeFile2 fn3 (pretty res1)
--            return ()
--
----        putIOwords ["expected res:", showT restest]
----    let res2p = bb2s . bl2b $ encodePretty restest
----    let res0p = bb2s . bl2b $ encodePretty res0
----    putIOwords ["pretty resp2", s2t res2p]
----    putIOwords ["pretty res0p", s2t res0p]
----    assertEqual res2p (show res0p)
------    assertEqual restest_nlpjson  (show res0)
--
--    assertEqual "" "XX"

--pretty :: (ToJSON a) => a -> Text
--pretty a = fromJustNote "pretty - issue x" . b2t $ ap
--    where
--        ap = bl2b $ encodePretty a  :: ByteString



---- show produces the "xx"
--test_1 = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:", take' 100 . showT $ f]
--        let r = decodeDoc1 f  :: Either String [Doc1]
--        putIOwords ["decoded:", showT r]
--        return r
--    assertEqual res (show res0)
--
--res =  ""


