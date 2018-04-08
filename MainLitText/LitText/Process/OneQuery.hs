 -----------------------------------------------------------------------------
--
-- Module      :  OneQuery
-- Copyright   :  andrew u frank -
--
-- | process one query
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Process.OneQuery (module Process.OneQuery
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Strings
import           Uniform.Error

import LitText.Foundation
--    (serverBrest, serverLocalhost
--            , rdfBase, dirQueries, PartURI, unPartURI
--           , getServer, LitTextFlags (..))
import Data.List.Split (chunksOf)
import Data.List.Utils (replace)
import Data.Either (isRight)
import CmdLineUtilities.UtilsProcessCmd
import CmdLineUtilities.UtilsProcessing
--import Process.UtilsParseArgs

ntExtension = Extension "nt"

oneQuery2 :: Inputs -> Path Abs File -> ErrIO Text
-- execute the query in fn against the fuseki store db in graph
-- db
oneQuery2 inp fn0 = do
        putIOwords ["oneQuery" , showT fn0]
        queryText :: [Text] <- read6  fn0 sparqlQueryFile
        -- cannot use the typedfile - would produce triples...
        when (isDebugFlag inp) $ putIOwords ["oneQuery db -  \n",  unlines' queryText ]


        let query2 =  unlines' queryText
        let graphDescription = maybe ""
                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
                (inGraph inp) :: Text
--        let auxgraphDescription = maybe ""
--                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
--                ( maux . inArgs $ inp) :: Text

        let query3 = s2t . replace "#_graphSource" (t2s graphDescription)
                        . t2s $ query2
--        let query4 = s2t . replace "#_auxgraphSource" (t2s auxgraphDescription)
--                    . t2s $ query3

--        when (inDebug inp) $ putIOwords ["oneQuery query processed \n",  query4,
--                    "\npathName", pathName  ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be
        -- needs to insert the graph in the query
        let pathName = (unPartURI $ inDB inp)  </> "sparql" -- "query"

        let fusekiServer = getServer  inp

        let query =  HttpVarParams [ ("output", Just "csv")]
        let appType = "application/sparql-query"

        resp <- callHTTP10post (isDebugFlag inp) appType  fusekiServer  pathName
                    (b2bl . t2b $ query3) query  (TimeOutSec $ inTimeOut inp)
--        resp <- post2store debug "application/sparql-query" fusekiServer pathName mgraph
--                (b2bl . t2b $ query4) [ ("output", Just "csv")] Nothing
--        resp <- makeHttpPost7 debug fusekiServer pathName
--                     [ ("output", Just "csv")]
--                        "application/sparql-query" query4

        let resp2 =   resp
        let resultExt = makeExtension . t2s $ ( (unPartURI $ inDB inp)  <.>  "csv") :: Extension
        let respFilename =  addExtension resultExt fn0
                    :: Path Abs File

        putIOwords ["oneQuery2 response\n",  resp2, "for", showT fn0]
        -- write the response into the flag file
        writeFileOrCreate2   respFilename resp2

        return resp2
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["oneQuery2  error caught 11", showT e
                , "\n filename is", showT fn0
                , "error is not raised again"]
        putIOwords ["oneQuery2 arguments were db - graph"
                , (unPartURI $ inDB inp), showT (inGraph inp), "file", showT fn0]
        return . unwords' $ ["oneQuery2 return after error", showT e]


oneQuery :: Bool -> URI -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Path Abs File -> ErrIO Text
-- execute the query in fn against the fuseki store db in graph
-- db
oneQuery debug server db mgraph maux timeout fn0 = do
        putIOwords ["oneQuery", "debug", showT debug, " db - graph", db, showT mgraph, "aux", showT maux
                    , "timeout", showT timeout, "file", showT fn0]
        queryText :: [Text] <- read6  fn0 sparqlQueryFile
        -- cannot use the typedfile - would produce triples...
        when debug $ putIOwords ["oneQuery db -  \n",  unlines' queryText ]


        let query2 =  unlines' queryText
        let graphDescription = maybe ""
                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
                mgraph :: Text
--        let auxgraphDescription = maybe ""
--                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
--                maux :: Text
            -- probably not required to process a aux graph
        let query3 = s2t . replace "#_graphSource" (t2s graphDescription)
                        . t2s $ query2
--        let query4 = s2t . replace "#_auxgraphSource" (t2s auxgraphDescription)
--                    . t2s $ query3

--        when debug $ putIOwords ["oneQuery query processed \n",  query4,
--                    "\npathName", pathName  ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be
        -- needs to insert the graph in the query
        let pathName = db </> "sparql" -- "query"

        let fusekiServer = getServer server

        resp <- post2store debug "application/sparql-query" fusekiServer pathName mgraph
                (b2bl . t2b $ query3)
                        (HttpVarParams [ ("output", Just "csv")])
                             Nothing
--        resp <- makeHttpPost7 debug fusekiServer pathName
--                     [ ("output", Just "csv")]
--                        "application/sparql-query" query4

        let resp2 =   resp
        let resultExt = makeExtension . t2s $ ( db <.>  "csv") :: Extension
        let respFilename =  addExtension resultExt fn0
                    :: Path Abs File

        putIOwords ["oneQuery response\n",  resp2, "for", showT fn0]
        -- write the response into the flag file
        writeFileOrCreate2   respFilename resp2

        return resp2
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["oneQuery  error caught 11", showT e
                , "\n filename is", showT fn0
                , "error is not raised again"]
        putIOwords ["oneQuery arguments were db - graph"
                , db, showT mgraph, "file", showT fn0]
        return . unwords' $ ["oneQuery return after error", showT e]


testfn0 = addFileName (dirQueries :: Path Abs Dir)
                             (t2s (("test" :: Text) </> ("test1" :: Text))  )
                                :: Path Abs File

test_1 = do
    r <- runErr $ oneQuery True serverBrest "testDB" (Just "t1") (Just "") Nothing testfn0
    assertBool (isRight r)

