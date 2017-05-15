
 -----------------------------------------------------------------------------
--
-- Module      :  Store.Virtuoso
--
-- | to access the virtuoso rdf database with sparql
--  operations for editing the triple store
-- make a class with the endpoint type as the only argument

-- as much as possible the same code as fourstore - merge later!
-----------------------------------------------------------------------------
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module xStore.Virtuoso(EndPointT
        , Virtuoso , virtuosoServer
--        , loadGraphV, loadGraphV2  -- both have permission issues
--        , insertGraphV

        , RDFaccess (..)
        , ByteString
            )  where


import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings

import qualified Data.ByteString.Char8   as B

import qualified Network.HTTP            as Net
import qualified Network.URI             as NetURI

import           Store.RDFstore.HttpCall -- (callHTTP3, parseURLchecked)
import Data.RDF.Triple2text (triple2text)
import           Store.StoreAlg          (EndPointT, RDFaccess (..), Triple)

data Virtuoso

virtuosoServer = (undef "virtuosoServer phantom":: Virtuoso)

instance RDFaccess Virtuoso ErrIO where
    selectQueryText _ = selectQueryTextv
    -- :: store ->  EndPoint -> Text -> ErrIO Text
--    insertTriplesIntoGraph :: store -> EndPointT -> [Triple] -> Maybe Text -> m Text
    insertTriplesIntoGraph _ = insertGraphV
    -- ^ insert the triples into the graph (if not Nothing)



-- | load a list of triples with Insert on /sparql endpoint
-- the maybe text is the graph name
insertGraphV :: EndPointT -> [Triple] -> Maybe Text -> ErrIO Text
insertGraphV ep tps mgr1 = do
    putIOwords ["insertGraphV virtuoso  ", "start",  ep ,   showT mgr1]
    let gr1 = maybe "http://gerastree.at/loadGraphx" id mgr1 :: Text

    let  uri      = "http://nlp.gerastree.at:8890/sparql/"
--            addSparql2endpoint ep
--    uriEnc <- parseURLchecked uri

    putIOwords ["insertGraphV virtuoso  ", "endpoint",  uri
                , "\ntriples", showT tps , "\n"
--                , "graph",  showT uri
                ]

    let triplesText = unlines' . map triple2text $ tps
--    let text = "SPARQL INSERT IN GRAPH "  <>  gr1 <>
--                " {" <> triplesText <> "}"
    let textWG = "INSERT DATA { GRAPH "  <>  gr1 <>
                " {" <> triplesText <> "} }"
    let textNG = "INSERT DATA"  <>
                " {" <> triplesText <> "  }"
    let text = textWG
    putIOwords ["insertGraphV  ", "url", showT ep, "\n\ttext",  text ]
    let textURL = s2t . Net.urlEncode . t2s $ text
--    let request = Net.postRequest  (t2s  ( uri <> "/?query=" <> textURL))
    -- this works!
--    let request = Net.postRequestWithBody (t2s uri) "application/sparql-query" (t2s text)
    let request = makeHTTPrequest5  Net.POST uri "application/sparql-update" text

    putIOwords ["insertGraphV before call HTTP5 - the uriEnc is ", showT uri
                , "\n\trequest", showT request
                , "\n\trequestbody",  s2t $ Net.rqBody request]
    response' <- callHTTP5  request
    putIOwords ["linsertGraphV before HTTP5 result is",  showT response']
    return response'


-- |Connect to remote 'EndPoint' and find all possible bindings for the
--  'Variable's in the query, given as a text.
-- returns text
-- is the same as in hsparql, except for the construction of the query
selectQueryTextv ::   EndPointT -> Maybe Text -> Maybe Text -> Text -> ErrIO Text
selectQueryTextv ep defgr namedgr q = do
    putIOwords ["selectQueryTextv", showT ep, showT defgr
                , showT namedgr, showT q]
    let  uri      = (addSparql2endpoint ep) <> "?"
                            <> urlEncodeVarsT [("query",   q),
                            ("default-graph-uri", maybe "" id  defgr)
                            ,("named-graph-uri", maybe "" id  namedgr)]
--                            ++ Net.urlEncodeVars [("query", t2s  q)]
--                            ++ "&" ++ Net.urlEncodeVars [("default-graph-uri", maybe "" t2s  defgr)]
--                            ++ "&" ++ Net.urlEncodeVars [("named-graph-uri", maybe "" t2s  namedgr)]
--                        , ("output", "sparql")]  -- should work, probably next version
--    putIOwords ["selectQueryTextv uri \n", uri]
    uriEnc <- parseURLchecked uri
--    putIOwords ["selectQueryTextv uriEnc  \n", show uriEnc]
    let h1 = Net.mkHeader Net.HdrAccept "application/sparql-results+xml"
        h2 = Net.mkHeader Net.HdrUserAgent "hsparql-client"
        request = Net.Request { Net.rqURI = uriEnc
                          , Net.rqHeaders = [h1,h2]
                          , Net.rqMethod = Net.GET
                          , Net.rqBody = "" :: String -- (""::B.ByteString)
                          -- only string or bytestring has instances for simpleHTTP
                          -- possibly the other queries should be changed similarly
                          }
    putIOwords ["selectQueryTextv t2s q \n",  q]
    putIOwords ["selectQueryTextv", uri]
    putIOwords ["selectQueryTextv uriEnc \n", showT uriEnc]
    response' <- callHTTP5  request
    putIOwords ["loadGraphx before HTTP5 result is",  showT response']
    return response'


addSparql2endpoint :: EndPointT -> Text
addSparql2endpoint ep = addFn ep  ("sparql/"::Text)


