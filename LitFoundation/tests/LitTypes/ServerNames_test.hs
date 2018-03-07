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


-- import           Data.RDF.Extension
--import Uniform.Strings
import Uniform.Error
--import Uniform.FileIO (makeAbsDir, makeRelDir)
--import Uniform.HttpCallWithConduit (makeAbsURI)
--import Uniform.HttpURI
--import Network.URI
import           Test.Framework
import Text.Read (Read (..))
import LitTypes.ServerNames

-- an attempt to have a read for URI  ReadS
--instance Read URI where
--    readsPrec _  = readS'

test_readURI = assertEqual localhost (read (show localhost))


test_makeURIok = assertEqual "\"http://nlp.gerastree.at\"" (showT  serverBrest)

test_makeURIfail = assertEqual "Nothing" (showT . parseAbsoluteURI $ "127.0.0.1")

ur1 = makeAbsURI "http://nlp.gerastree.at" :: URI
ur1s = show ur1 :: String

test_re1 = assertEqual ur1 (read ur1s)
