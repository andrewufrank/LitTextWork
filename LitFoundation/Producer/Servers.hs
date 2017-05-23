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

module Producer.Servers (
    module Producer.Servers
    , module Network.URI
    , module Uniform.HttpGet
    ) where


-- import           Data.RDF.Extension
import Uniform.Strings
import Uniform.Error
import Uniform.HttpGet
import Network.URI
import           Test.Framework

--type PartURI = Text   -- should be defined in uniform.http?? todo
        -- is defined in RDF.Extension

serverLocalhost, serverBrest :: URI

serverLocalhost = localhost
serverBrest = makeAbsURI "http://nlp.gerastree.at"

localhost = makeAbsURI "http://127.0.0.1"

-- -- todo move to uniform-http
-- makeAbsURI :: Text -> URI
-- makeAbsURI u = maybe (errorT ["makeURI in Foundation Servers", u])
--                 id
--                 (parseAbsoluteURI . t2s $ u)
-- makeURI :: Text -> URI
-- makeURI u = maybe (errorT ["makeURI in Foundation Servers", u])
--                 id
--                 (parseURI . t2s $ u)

-- test_makeURIok = assertEqual "" (showT  serverBrest)
-- test_makeURIfail = assertEqual "" (showT . parseAbsoluteURI $ "127.0.0.1")
