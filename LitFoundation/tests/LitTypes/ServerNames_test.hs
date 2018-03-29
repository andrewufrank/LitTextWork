-----------------------------------------------------------------------------
--
-- Module      :  Foundation - servers
-- Copyright   :  andrew u frank -
--
-- | the addresses of the servers
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LitTypes.ServerNames_test where


import Uniform.Error
import           Test.Framework
import Text.Read (Read (..))
import LitTypes.ServerNames


test_readURI = assertEqual localhost (read (show localhost))


test_makeURIok = assertEqual "\"http://nlp.gerastree.at\"" (showT  serverBrest)

test_makeURIfail = assertEqual "Nothing" (showT . parseAbsoluteURI $ "127.0.0.1")

ur1 = makeAbsURI "http://nlp.gerastree.at" :: URI
ur1s = show ur1 :: String

test_re1 = assertEqual ur1 (read ur1s)
