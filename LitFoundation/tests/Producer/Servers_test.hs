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

module Producer.Servers_test where


-- import           Data.RDF.Extension
import Uniform.Strings
import Uniform.Error
--import Uniform.FileIO (makeAbsDir, makeRelDir)
--import Uniform.HttpCallWithConduit (makeAbsURI)
--import Uniform.HttpURI
--import Network.URI
import           Test.Framework
import Text.Read (Read (..))
import Producer.Servers

-- an attempt to have a read for URI  ReadS
instance Read URI where
    readsPrec _  = readS'

test_readURI = assertEqual localhost (read (show localhost))


test_makeURIok = assertEqual "" (showT  serverBrest)

test_makeURIfail = assertEqual "" (showT . parseAbsoluteURI $ "127.0.0.1")
