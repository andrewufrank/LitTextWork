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


test_makeURIok = assertEqual
        "ServerURI {unServerURI = \"http://nlp.gerastree.at\"}"
         (showT  serverBrest)

test_makeURIlocalHost = assertEqual
        "ServerURI {unServerURI = \"http://127.0.0.1\"}"
                (showT . mkServerURI $ "http://127.0.0.1")

--test_make_MUST_FAIL = assertEqual  "MUST FAIL"
--                (showT . mkServerURI $ "127.0.0.1")

ur1 = mkServerURI "http://nlp.gerastree.at" :: ServerURI
ur1s = show ur1 :: String

test_re1 = assertEqual ur1 (read ur1s)
