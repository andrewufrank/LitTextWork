{-----------------------------------------------------------------------------
--
-- Module      :  Main
-- @Author@
--
-- |  a package to access the fuseki server endpoint sparql query und update
-- port is 3030

-- to start server see https://jena.apache.org/documentation/serving_data/#download-fuseki1
-----------------------------------------------------------------------------}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main  where

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
--import EitherError.Top
--import Data.Strings
import qualified Control.Exception        as E (catch)
import           Data.RDF.Types           (RDF (..))
import qualified Data.Text                as T (lines)
--import           Store.RDFstore.Checks
import           Store.RDFstore.Structure
import Data.RDF.Extension (mkTripleLang, mkTripleRef, LanguageCode (..))
import Data.RDF.Triple2text (triple2text)
import           Store.Fuseki
import           Store.StoreAlg
--import Data.RDF.Extension
--import Text2Onto.TripleMaker
import           Data.RDF


programName = "fuseki with jena tbc store"
progTitle =  "testing the sparql insert"

mainDebug = False

u1 :: Text
-- an insert SPARUL query
u1 = unwords' [  --`"PREFIX dc: <http://purl.org/dc/elements/1.1/>" ,
              "INSERT DATA"
            , "{  GRAPH <http://example/store> {"
            , "  <http://example/book1> " -- dc:title \"A new book\" ; "
            , "             <http://example/creator> \"A.N.Other\" ."
            , " } }"
            ]

u1triplesOnly :: [Triple]
u1triplesOnly =  [mkTripleLang English "http://example/book1"  "http://example/creator" "A.N.Other"]


test1q :: ErrIO ()
test1q = do
    putIOwords ["test1q start"]
    t1 <- selectQueryText fusekiServer fusekiQueryServer
                                 Nothing  -- default named graph
                                 Nothing -- named graph
                                 testQueryFor2
    putIOwords ["test1q result on demo update", showT t1]
    return ()



testSparqlInsert :: ErrIO ()
-- test load with sparl insert
-- gives error - 500 not permitted
-- gives 403 insufficient permission
testSparqlInsert = do
    putIOwords ["testPost2 start"]
    t1 <- insertTriplesIntoGraph fusekiServer fusekiUpdateServer
                                 oneTriple (Just "<http://example/store>")
    putIOwords ["testPost2 result on demo update", showT t1]
    return ()

mainErrIO ::   ErrIO ()
mainErrIO   = do
--    test1q
--    test2l
--    testPost2
    testSparqlInsert
    return ()

main :: IO ()
main = do
        putIOwords [ "------------------ ", programName , progTitle, " ----------------------------"]
        r <- runErr $ mainErrIO

        putIOwords ["main", progTitle, "returning", showT $ r, "-------------------------"]
--    `catchError`
--        \(e) -> throwError . unwords $ [ "SomeIOerror", "final error", show e]


-------------------------------------------------------------testing
fusekiQueryServer = "http://127.0.0.1:3030/dataset/query"
fusekiUpdateServer = "http://127.0.0.1:3030/MyDataset/update"
-- this is open for update - how to protect?


