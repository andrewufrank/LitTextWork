
 -----------------------------------------------------------------------------
--
-- Module      :  Store.Fuseki
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

module Store.Fuseki(EndPointT
        , Fuseki , fusekiServer
--        , Virtuoso, virtuosoServer
        , ShowServer (..)
--        , loadGraphF, loadGraphF2  -- both have permission issues
--        , insertGraphF

        , RDFaccess (..)
        , ByteString
            )  where


import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
import Uniform.HttpGet

import qualified Data.ByteString.Char8   as B

--import qualified Network.HTTP            as Net
--import qualified Network.URI             as NetURI

--import           Store.RDFstore.HttpCall -- (callHTTP3, parseURLchecked)
import Data.RDF.Triple2text (triple2text)
import           Store.StoreAlg          (EndPointT, RDFaccess (..), Triple)


--debugFuseki =  False

data Virtuoso
data Fuseki

class ShowServer f where
    showServer :: f -> Text
instance ShowServer Fuseki where showServer _ = "Fuseki"
instance ShowServer Virtuoso where showServer _ = "Virtuoso"

--virtuosoServer = (undef "virtuosoServer phantom":: Virtuoso)

fusekiServer = (undef "fusekiServer phantom":: Fuseki)

instance RDFaccess Fuseki ErrIO where
--    selectQueryText :: store ->  EndPointT -> Maybe Text -> Maybe Text -> Text -> m Text
    -- ^ process a select query (and other queries which use GET
    -- give a default and a name graph descriptor (if any)
--    selectQueryText _ = selectQueryTextF
    -- :: store ->  EndPoint -> Text -> ErrIO Text

--    insertTriplesIntoGraph :: store -> EndPointT -> [Triple] -> Maybe Text -> m Text
    insertTriplesIntoGraph _ = insertTriplesIntoGraphF False
    -- ^ insert the triples into the graph (if not Nothing)


textWG triplesText gr1 = "INSERT DATA { GRAPH "  <>  gr1 <>
                " {" <> triplesText <> "} }"
textNG triplesText = "INSERT DATA"  <>
                " {" <> triplesText <> "  }"

-- | load a list of triples with Insert on /sparql endpoint
-- the maybe text is the graph name
insertTriplesIntoGraphF :: Bool -> EndPointT -> [Triple] -> Maybe Text -> ErrIO Text
insertTriplesIntoGraphF debugFuseki ep tps mgr1 = do
    when debugFuseki $ putIOwords ["insertGraphF fuseki  ", "start",  ep ,   showT mgr1]

--    when debugFuseki $ putIOwords ["insertGraphF fuseki  ", "endpoint",  ep
----                , "\ntriples", unlines' . map showT $ tps , "\n"
--                ]

    let triplesText = unlines' . map triple2text $ tps

    let text = maybe (textNG triplesText)  (textWG triplesText . wrapInSpitz) mgr1

    when debugFuseki $
            putIOwords ["insertGraphF with text ", "url", showT ep, "\n\ttext",  text ]

--    let nlpProcesses = [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                    -- removed ,coref
--                    , ("outputFormat","xml")
--                    ]

    response' <- makeHttpPost7 debugFuseki ep [] "application/sparql-update" text

--    let request = makeHTTPrequest5  POST  ep "application/sparql-update" text

--    when debugFuseki $

--    when debugFuseki $
--            putIOwords ["insertGraphF before call HTTP5 - the uriEnc is: ", showT ep
--                , "\n\trequest", showT request
--                , "\n\trequestbody",  bb2t $ Net.rqBody request]

--    response' <- callHTTP7 debugFuseki request

    when debugFuseki $ putIOwords ["insertGraphF result is:",  showT response']

    return response'


--bb2t :: ByteString -> Text
--bb2t = fromJustNote "bytestring conversionm bb2t" . b2t

-- |Connect to remote 'EndPoint' and find all possible bindings for the
--  'Variable's in the query, given as a text.
-- returns text
-- is the same as in hsparql, except for the construction of the query
--selectQueryTextF ::   EndPointT -> Maybe Text -> Maybe Text -> Text -> ErrIO Text
--selectQueryTextF ep defgr namedgr q = do
--    when debugFuseki $ putIOwords ["selectQueryTextF", showT ep, showT defgr
--                , showT namedgr, showT q]
----    let  uri      = t2s ep
--    when debugFuseki $ putIOwords ["selectQueryTextv uri \n", ep]
----             http://localhost:3030/dataset/query?query=select+%2A+%7B%3Fs+%3Fp+%3Fq%7D+limit+10
--
--    let request = makeHTTPgetrequestNoBody ep "?query=" q
--    response' <- callHTTP5  request
--    when debugFuseki $     putIOwords ["selectQueryTextF call HTTP5 result is",  showT response']
--    return response'


--addSparql2endpoint :: EndPointT -> Text
--addSparql2endpoint ep = addFn ep  ("sparql/"::Text)






