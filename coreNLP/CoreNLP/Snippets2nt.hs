{-----------------------------------------------------------------------------
--
-- Module      :  convert the text snippets (hl1 pieces) to nt with coreNLP
-- and store the RDF triples with SPARQL insert
--
-- |
--
-----------------------------------------------------------------------------}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}
--{-# LANGUAGE DeriveAnyClass        #-}
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings     #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
--{-# LANGUAGE TypeSynonymInstances  #-}

module CoreNLP.Snippets2nt (
       module CoreNLP.Snippets2nt
        , htf_thisModulesTests
--        , makeNLPrequest5
--        ,makeNLPrequest6
        , readDocString
        -- , nlp_serverLoc, host_serverLoc
        )  where

import           Test.Framework

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
import           Uniform.Zero
import Uniform.HttpGet

import           CoreNLP.CoreNLPxml

import           Data.RDF                (Triple, mkRdf, triplesOf)
import           Data.RDF.Extension      (gerastreeURI, PartURI)
import           Data.RDF.FileTypes      (RDFgraph (..), ntFile, unRDFgraph)
import           Data.RDF.Prefs          (prefixs2pm, startPrefix)

--import           CoreNLP.Defs0
-- import           Store.Fuseki
--import           Store.RDFstore.HttpCall (RequestMethod (..)
----                                      , callHTTP6,
----                                          makeHTTPrequest6
--                                          , parseURLchecked,
--                                          callHTTP7, makeHTTPrequest5,
--                                          urlEncodeVarsT)
-- import           Store.StoreAlg
--import Store.Fuseki (fusekiServer, ShowServer (..))
--import           Store.RDFstore.HttpCall
import           Data.Text.Encoding
import           Text.XML.HXT.Core       hiding (when)

-- debugParse = False
-- loadGraphDebug = False
--
-- showXML = True
-- produceFiles = False
--
-- nlp_serverLoc = "http://nlp.gerastree.at" :: PartURI
-- host_serverLoc = "http://127.0.0.1" :: PartURI
-- serverLoc = "nlp.gerastree.at"
-- localHost = "127.0.0.1"
--
-- nlpServerEnglish = "http://" <> serverLoc <> ":9000"  -- not localhost!
-- --nlpServer = "http://nlp.gerastree.at:9000"
-- nlpServerGerman = "http://" <> serverLoc <> ":9001"  -- for german
-- nlpServerNone = nlpServerEnglish -- for no language which can be processed
-- -- should be a server just returning the input tokenized etc
--
-- --sparqlServer = "http://" <> serverLoc <> ":8890" -- virtuoso
-- --sparqlServer = "http://" <> serverLoc <> ":3030/march17/update"  -- fuseki
-- -- sparqlServerBrest = "http://" <> serverLoc <> ":3030/march25/update"  -- fuseki brest
-- --sparqlServer = "http://" <> localHost <> ":3030/marchDB/update"  -- fuseki
-- -- change the dirname and mkdir for it, but leave web address the same
--
-- pm = prefixs2pm startPrefix

-- could be done in one call, but too complicated
-- readDocHTTP :: Text  -> ErrIO  Doc0
-- -- read the doc by http call
-- readDocHTTP fp = do
--   docs  :: [Doc0] <-callIO $ do
--     d1 :: [Doc0] <-  runX (readDocument
--         [withValidate no
--         , withCurl ["properties=
--             {"annotators":"tokenize,ssplit,pos,lemma,ner,parse,dcoref",
--             "outputFormat":"xml"}""]
--         ] "localhost:9000"
--                             >>> getDoc0)   -- getToken1)
--     return d1

readDocString :: Bool -> Text  -> ErrIO  Doc0
readDocString showXML text = do
  docs  :: [Doc0] <-callIO $ do
        d1 :: [Doc0] <-  runX (readString [withValidate no]  (t2s text)
                                >>> getDoc0)
        return d1
  when showXML $ do
      putIOwords ["the xml formated"]
      res <- callIO $ runX . xshow $ readString [withValidate no]  (t2s text)
                                        >>> indentDoc
      putIOwords  $ map s2t res
      putIOwords ["the xml formated ---------------------"]

--  let toks2 = filter ((0 /=). sid) toks
  -- seems to add a lot of empty sentences

  if (length docs) > 1
        then error "multiple document tags"
        else  do
--            putIOwords ["readDocString - the xml read"]
            return (headNote "no document found" docs)
            -- error in case of 0


--debugNLPrequest = True -- to produce debug output -

-- added to deal with the text to bytestring conversio per caller
-- but effectively not required for corenlp german - utf8 seem to work
--makeNLPrequest6 :: Bool -> Text -> ByteString  -> ErrIO ( ByteString) -- Http.Request B.ByteString
---- make a POST request to coreNLP server
---- no errors are precessed in callHTTP6
--makeNLPrequest6 debugNLPrequest dest body = do
--        -- when debugNLPrequest $
--        putIOwords ["makeNLPrequest   with \n", showT (show body)]
--        -- to show the character codes to identify strange ones
--        let  uri      = dest </> "?" <>
--                urlEncodeVarsT
--                    [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
--                    , ("outputFormat","xml")
--                    ]
--        when debugNLPrequest $ putIOwords ["makeNLPrequest uri", uri ]
--        uriEnc <- parseURLchecked uri  -- just testing
--        let req = makeHTTPrequest6 POST uri "application/x-www-form-urlencoded" body
--        when debugNLPrequest $ putIOwordsT ["makeNLPrequest request", showT req ]
--
--        response <- callHTTP6 False req  -- bool controls debug output
--        when debugNLPrequest $ putIOwords ["makeNLPrequest response", bb2t response, "from", dest ]
--
--        return response

--makeNLPrequest5 :: Bool -> Text -> Text  -> ErrIO ( Text) -- Http.Request B.ByteString
---- make a POST request to coreNLP server
---- call and return with text
--makeNLPrequest5 debugNLPrequest dest body = do
--        -- when debugNLPrequest $
--        putIOwords ["makeNLPrequest   with \n", body]
--
--        response <- makeHttpPOST7 deubNLPrequest dest  mime
--                    [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
--                    , ("outputFormat","xml")
--                    ]
--                    body

--        when debugNLPrequest $ putIOwords ["makeNLPrequest uri", uri ]
--        uriEnc <- parseURLchecked uri  -- just testing
--        let req = makeHTTPrequest5 POST uri "application/x-www-form-urlencoded" body
--        when debugNLPrequest $ putIOwordsT ["makeNLPrequest request", showT req ]
--
--        response <- callHTTP7 False req  -- bool controls debug output
--        when debugNLPrequest $ putIOwords ["makeNLPrequest response",  response, "from", dest ]

--        return response

---- not working with this naive approach
--makeTTrequest :: Bool -> Text -> Text -> ErrIO (Maybe Text) -- Http.Request B.ByteString
---- make a POST request to coreNLP server
--makeTTrequest debugNLPrequest dest text = do
--        when debugNLPrequest $
--            putIOwords ["makeTTrequest start with", text]
--        let  uri      = dest
--              -- </> "?" <>
--              --   urlEncodeVarsT
--              --       [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--              --       -- removed ,coref
--              --       , ("outputFormat","xml")
--              --       ]
--        when debugNLPrequest $ putIOwords ["makeTTrequest uri", uri ]
--        uriEnc <- parseURLchecked uri  -- just testing
--        let req = makeHTTPrequest5 GET uri "application/x-www-form-urlencoded" text
--        when debugNLPrequest $ putIOwords ["makeNLPrequest request", showT req ]
--
--        response <- callHTTP5 req
--        when debugNLPrequest $ putIOwords ["makeTTrequest response", response, "from", dest ]
--
--        return (Just response)
