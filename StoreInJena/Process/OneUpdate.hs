 -----------------------------------------------------------------------------
--
-- Module      :  OneUpdate
-- Copyright   :  andrew u frank -
--
-- | process one update (insert,delete,modify) sparql
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Process.OneUpdate (module Process.OneUpdate
        , URI
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Strings
import           Uniform.Error

--import Uniform.HttpGet (makeHttpGet7, addPort2URI)
import Uniform.HttpCall (addPort2URI, URI
            ,  HttpVarParams(..))
import Data.RDFext.Extension (ntFileTriples, sparqlUpdateFile)
import LitTypes.TextDescriptor (serverBrest, rdfBase, dirQueries)
import Data.List.Split (chunksOf)
import Data.List.Utils (replace)

ntExtension = Extension "nt"
--flagExtension a = Extension . t2s $ ("ntFlag_" <> a)

--destBrestFuseki =  addPort2URI  serverBrest 3030

insertData :: Maybe Text -> Text -> Text
-- make an insert data query with a graph name or default
insertData mgraph trips = insertDataText
           <> graphText mgraph <> trips <> closeText
    where
        insertDataText = "INSERT DATA {"
        graphText  = maybe "{"
                    (\a ->  " GRAPH <" <> (showT rdfBase </> a) <> "> {")

        closeText = "} }"

oneUpdate :: Bool -> URI -> Text -> Maybe Text -> Maybe Text ->  Text
        -> Path Abs File ->  ErrIO Text
-- execute the query in fn against the fuseki store db in graph
-- and put result in mdest graph
oneUpdate debug server db mgraph mdest wordnetgrah fn = do
        when True $ putIOwords ["oneUpdate db - graph", db, showT mgraph
                    , "file", showT fn  --, "debug", showT debug
                    , "\n----------------------" ]

        when True $ putIOwords ["oneUpdate db - graph", db, showT mgraph, "file", showT fn]

        let server2 = addPort2URI  server 3030
--        let fn2 =  setExtension sparqlQueryFile $ fn
--        let  fn0 = addFileName dirQueries   fn  :: Path Abs File
        let  fn0 =     fn  :: Path Abs File
        queryText :: [Text] <- read6  fn0 sparqlUpdateFile
        -- cannot use the typedfile - would produce triples...
        when True $ putIOwords ["oneUpdate db -  \n",  unlines' queryText ]

        -- needs to insert the graph in the query
        -- needs a second graph in which to insert

        let query2 = unlines' queryText
        let graphDescription = maybe ""
--                (\a -> concat' ["Graph   <", showT rdfBase, "/", a, ">"])
                (\a -> concat' ["  <", showT rdfBase, "/", a, ">"])
--                -- goes to using clause without GRAPH
                mgraph :: Text
--        let sourceDescription = maybe ""
--                (\a -> concat' ["Graph   <", showT rdfBase, "/", a, ">"])
--                mgraph :: Text
        let destDescription = maybe ""
                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
                mdest :: Text
        let query3 = s2t . replace "#_graphSource" (t2s graphDescription)
                    . t2s $ query2
        let query4 = s2t . replace "#_graphDest" (t2s destDescription)
                    . t2s $ query3
        let query5 = s2t . replace "#_graphWordnet"
                        (t2s . wrapInSpitz $ wordnetgrah)  . t2s $ query4
        let pathName = db  </> "update" -- "query"

        when debug $ putIOwords ["oneUpdate: oneQuery query processed \n",  query5,
                    "\npathName", pathName ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be
        resp <- callHTTP10post debug "application/sparql-update"  
                    server2 pathName zero query5 (TimeOutSec $ Just 300)
        -- resp <- makeHttpPost7 False server2 pathName
        --              zero -- (HttpVarParams [])
        --                 "application/sparql-update" query5

        let resp2 =   resp

        putIOwords ["oneUpdate response\n",  resp2, "for", showT fn]

        return resp2
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["oneUpdate  error caught 11",  e
                , "\n filename is", showT fn
                , "error is not raised again"]
        putIOwords ["oneUpdate arguments were db - graph"
                , db, showT mgraph, "file", showT fn]
        return . unwords' $ ["oneUpdate return after error",  e]

{- example and test query test6.query
    insert  {  #_graphDest
                { ?para1 lit:test1 ?count1  .
                }
    }

WHERE
 {Select (count (?tok)  AS ?count1) (sample (?para) as ?para1)
        where
            { #_graphSource
                {
                ?tok nlp:lemma3 ?lem .
                ?tok rdfs:partOf ?sent .
                ?sent rdfs:partOf ?para .
                }
            } group by ?para
  }
  -}


db1 = "corpus28"

graph1 = Just "t1" -- Just "http://gerastree.at/h" -- http://server/unset-base/
fn1 = makeAbsFile "/home/frank/additionalSpace/DataBig/Queries/test6"

test_1 = do
    r <- runErr $ oneUpdate True serverBrest "corpus28" (Just "t1") (Just "r6")
                "http://nlp.gerastree.at:3030/corpus3/data/wn31" -- not used ...
             (addFileName dirQueries $ makeRelFile "test6")
    putStrLn ("response \n" ++ show r)
    assertEqual (Right "") r

