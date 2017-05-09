{-----------------------------------------------------------------------------
--
-- Module      :  Main
-- @Author@
--
-- |  a package to access the virtuosoe endpoint sparql query und update
-- port is 8890
-- uses virtuoso on a jessie (not on stretch)

-- differences vs 4store
-- the endpoint is one, the files are selected


-----------------------------------------------------------------------------}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main  where

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
import           Uniform.Convenience.StartApp

--import EitherError.Top
--import Data.Strings
import qualified Control.Exception        as E (catch)
import           Data.RDF.Types           (RDF (..))
import qualified Data.Text                as T (lines)
--import           Store.RDFstore.Checks
import           Store.RDFstore.Structure
import Data.RDF.Extension (mkTripleLang, mkTripleRef, LanguageCode (..))
import Data.RDF.Triple2text (triple2text)
--import           Store.Virtuoso
import           Store.Fuseki
--import Data.RDF.Extension
--import Text2Onto.TripleMaker
import           Data.RDF
import           Store.StoreAlg


programName = "virtuoso in store"
progTitle =  "testing the virtuoso insert"

mainDebug = False

--u1 :: Text
---- an insert SPARUL query
--u1 = unwords' [  --`"PREFIX dc: <http://purl.org/dc/elements/1.1/>" ,
--              "INSERT DATA"
--            , "{  GRAPH <http://example/store> {"
--            , "  <http://example/book1> " -- dc:title \"A new book\" ; "
--            , "             <http://example/creator> \"A.N.Other\" ."
--            , " } }"
--            ]

u1triplesOnly :: [Triple]
u1triplesOnly =  [mkTripleLang English "http://example/book1"  "http://example/creator" "A.N.Other"]

u2triple = [mkTripleRef "http://demo.openlinksw.com/DAV/home/demo_about.rdf"
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                    "http://rdfs.org/sioc/ns#User"]

test1q :: ErrIO ()
test1q = do
    let server = fusekiServer
    let endpoint = fusekiQueryServerBrest
    putIOwords ["test1q start", showServer server, endpoint ]
--    t1 <- selectQueryText virtuosoServer sparqlServer
--    t1 <- selectQueryText fusekiServer sparqlQueryServer
    t1 <- selectQueryText server endpoint
                                 Nothing  -- default named graph
                                 Nothing -- named graph
                                 testQueryFor50
    putIOwords ["test1q result on demo query", showT t1]
    return ()

test1qg :: ErrIO ()
-- select with a graph name
test1qg = do
    let server = fusekiServer
    let endpoint = fusekiQueryServerBrest
    putIOwords ["test1q start", showServer server, endpoint ]
--    t1 <- selectQueryText virtuosoServer sparqlServer
--    t1 <- selectQueryText fusekiServer sparqlQueryServer
    t1 <- selectQueryText server endpoint
                                 Nothing  -- default named graph
                                 Nothing -- named graph
                                 (testQueryGraph exampleGraph)
    putIOwords ["test1q result on demo query", showT t1]
    return ()

exampleGraph =  "http://example/store" :: Text

testSparqlInsert :: ErrIO ()
-- test load with sparl insert
-- gives error - 500 not permitted
-- gives 403 insufficient permission
testSparqlInsert = do
    let server = fusekiServer
    let endpoint = fusekiUpdateServerBrest
    putIOwords ["testSparqlInsert start", showServer server, endpoint ]
--    t1 <- insertTriplesIntoGraph virtuosoServer sparqlServer
--    t1 <- insertTriplesIntoGraph fusekiServer sparqlQueryServer
--    t1 <- insertTriplesIntoGraph fusekiServer fusekiUpdateServer
    t1 <- insertTriplesIntoGraph fusekiServer fusekiUpdateServerBrest
                                 ( u2triple) (Just exampleGraph)
    putIOwords ["testSparqlInsert result on demo insert", showT t1]
    return ()

mainErrIO ::   ErrIO ()
mainErrIO   = do
    testSparqlInsert
    test1qg
    return ()

main :: IO ()
main = startProg programName progTitle mainErrIO


-------------------------------------------------------------testing

-- do not use
-- fuseki local host
fusekiQueryServer = "http://127.0.0.1:3030/MyDataset/query" :: Text
fusekiUpdateServer = "http://127.0.0.1:3030/MyDataset/update" :: Text

-- fuseki local host
fusekiQueryServerMarch13 = "http://127.0.0.1:3030/march13/query" :: Text
fusekiUpdateServerMarch13 = "http://127.0.0.1:3030/march13/update" :: Text

---- fuseki brest in memory
--fusekiQueryServerBrest = "http://nlp.gerastree.at:3030/DS1/query" :: Text
--fusekiUpdateServerBrest = "http://nlp.gerastree.at:3030/DS1/update" :: Text

-- fuseki brest march13
fusekiQueryServerBrest = "http://nlp.gerastree.at:3030/march13/query" :: Text
fusekiUpdateServerBrest = "http://nlp.gerastree.at:3030/march13/update" :: Text

--virtuoso brest
sparqlServer = "http://nlp.gerastree.at:8890":: Text
sparqlQueryServer = "http://nlp.gerastree.at:8890/sparql":: Text
-- this is open for update - how to protect


