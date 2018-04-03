 -----------------------------------------------------------------------------
--
-- Module      :  OneUpdate
-- Copyright   :  andrew u frank -
--
-- | delete one graph, but fuseki does still show the triples,
-- eventhough they cannot be found
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Process.DeleteOneGraph (module Process.DeleteOneGraph
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Strings
import           Uniform.Error

--import Uniform.HttpGet (makeHttpGet7, addPort2URI)
import Uniform.HttpCall (callHTTP8post, addPort2URI, makeHttpPost7, URI)
--import Data.RDF.FileTypes (ntFileTriples, sparqlUpdateFile)
import LitTypes.TextDescriptor (serverBrest, rdfBase, dirQueries)
import Data.List.Split (chunksOf)
import Data.List.Utils (replace)

ntExtension = Extension "nt"
--flagExtension a = Extension . t2s $ ("ntFlag_" <> a)

destBrestFuseki =  addPort2URI  serverBrest 3030

deleteGraphText ::  Text ->   Text
-- make an insert data query with a graph name or default
deleteGraphText  graph   = unlines' [
    " WITH  <" <> (showT rdfBase </> graph) <> "> "
    , "  DELETE   { ?o ?p ?q .}  "
    , "  WHERE { "  -- ?o ?p ?q1 . "
--    , "      FILTER (?q1 = \"test\"). "
    , "      ?o ?p ?q .} "
    ]

{-
    DELETE
 { ?book ?p ?v }
WHERE
 { ?book dc:date ?date .
   FILTER ( ?date > "1970-01-01T00:00:00-02:00"^^xsd:dateTime )
   ?book ?p ?v
 }
 -}

deleteOneGraph :: Bool -> URI -> Text ->  Text  ->  ErrIO Text
-- execute the query in fn against the fuseki store db in graph
-- and put result in mdest graph
deleteOneGraph debug server db graph   = do
        when False $ putIOwords ["deleteOneGraph db - graph", db,  graph ]

        let query4 =  deleteGraphText graph
        let pathName = db  </> "update" -- "query"

        when debug $ putIOwords ["deleteOneGraph query processed \n",  query4,
                    "\npathName", pathName ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be
--        resp <- return ""
        resp <- makeHttpPost7 False server pathName
                 zero --    (HttpVarParams [ ])
                        "application/sparql-update" query4

        let resp2 =  resp

        putIOwords ["deleteOneGraph response\n",  resp2, "for", graph]
        -- write the response into the flag file

        return resp2
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["deleteOneGraph  error caught 11", showT e
                , "\n graph nme is", graph
                , "error is not raised again"]
        putIOwords ["deleteOneGraph arguments were db - graph"
                , db, showT graph, "file"]
        return . unwords' $ ["deleteOneGraph return after error", showT e]
--        fail . unwords $  [ "callHTTP8post httperror 3", show e]

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

-- needs adaptation to current graph and existing db
--test_1 = do
--    r <- runErr $ deleteOneGraph True serverBrest "corpus28" "r5"
--    putStrLn ("response \n" ++ show r)
--    assertEqual (Right "") r

