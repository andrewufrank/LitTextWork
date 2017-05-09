
 -----------------------------------------------------------------------------
--
-- Module      :  Store.StoreAlg
--
-- | to access sparql endpoint operations for editing the triple store
-- different endpoint can be accomodated

-- includes the test queries etc.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Store.StoreAlg (EndPointT
        , RDFaccess (..)
        , ByteString
        , PrefixMappings
        , Triple (..)
        , testQueryFor2, testQueryFor50
        -- , oneTriple
        , testQueryGraph
       )  where


import           Data.RDF           (PrefixMappings, Triple)
import qualified Data.RDF           as RDF
import           Data.RDF.Extension (LanguageCode (..), mkTripleLang,
                                     mkTripleRef)
import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings

type EndPointT = Text   -- copies hsparql


-- | access and changing data in a RDF store
-- the first parameter selects the store
-- and for each brand of store a different instance can be given
-- (could be bundled with the endpoint definition)
--
-- missing: ask and construct queries specifically
--          other encodings of the files

class RDFaccess store m where
    selectQueryText :: store ->  EndPointT -> Maybe Text -> Maybe Text -> Text -> m Text
    -- ^ process a select query (and other queries which use GET
    -- give a default and a name graph descriptor (if any)
    insertTriplesIntoGraph :: store -> EndPointT -> [Triple] -> Maybe Text -> m Text
    -- ^ insert the triples into the graph (if not Nothing)


--    updateText ::  store -> EndPoint -> Text -> m Text
--    -- ^ process an update sparql query (goes to POST)
--    deleteGraph :: store -> EndPoint -> Maybe String -> m Text
--    -- ^ delete the named graph from the store
--    loadGraph :: store -> EndPointT -> [Triple] -> Maybe Text -> m Text
   -- ^ load (replace) the   triples  into the graph
   -- Maybe String gives the graph
   -- use INSERT
   -- process with POST

--    appendGraph :: store -> EndPoint -> FilePath -> Maybe String -> m Text
--    -- ^ append the content of the file (turtle encoded) into the graph
--    unloadGraph :: store -> EndPoint -> FilePath ->
--                Maybe String -> RDF.PrefixMappings -> m ()
--    -- ^ extract a graph into a turtle format which can be loaded
--    constructQuery :: store -> EndPoint -> FilePath -> m Text
--    -- ^ execute a construct query (must go to POST)
--    dumpKB :: store -> EndPoint -> FilePath -> m Text
--    -- ^ extract the full kb
--
--    appendTurtle :: store -> EndPoint -> Text -> Maybe String -> m Text
--    -- ^ append the text (turtle encoded) into the graph

---- tests

testQueryFor2 :: Text
testQueryFor2 = unwords' [ "SELECT * WHERE {?s ?p ?o} limit 2"]

testQueryFor50 :: Text
testQueryFor50 = unwords' [ "SELECT * WHERE {?s ?p ?o} limit 50"]

testQueryGraph :: Text -> Text
testQueryGraph gr = unwords' [ "SELECT * from ", wrapInSpitz gr, " WHERE {?s ?p ?o} limit 2"]

-- oneTriple = [mkTripleRef "http://demo.openlinksw.com/DAV/home/demo_about.rdf"
--                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
--                     "http://rdfs.org/sioc/ns#User"]

---- do not use
---- fuseki local host
--fusekiQueryServer = "http://127.0.0.1:3030/MyDataset/query" :: Text
--fusekiUpdateServer = "http://127.0.0.1:3030/MyDataset/update" :: Text
--
---- fuseki local host
--fusekiQueryServerMarch13 = "http://127.0.0.1:3030/march13/query" :: Text
--fusekiUpdateServerMarch13 = "http://127.0.0.1:3030/march13/update" :: Text
--
------ fuseki brest in memory
----fusekiQueryServerBrest = "http://nlp.gerastree.at:3030/DS1/query" :: Text
----fusekiUpdateServerBrest = "http://nlp.gerastree.at:3030/DS1/update" :: Text
--
---- fuseki brest march13
--fusekiQueryServerBrest = "http://nlp.gerastree.at:3030/march13/query" :: Text
--fusekiUpdateServerBrest = "http://nlp.gerastree.at:3030/march13/update" :: Text
--
----virtuoso brest
--sparqlServer = "http://nlp.gerastree.at:8890":: Text
--sparqlQueryServer = "http://nlp.gerastree.at:8890/sparql":: Text
---- this is open for update - how to protect
