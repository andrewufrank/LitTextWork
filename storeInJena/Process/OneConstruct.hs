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


module Process.OneConstruct (module Process.OneConstruct
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Strings
import           Uniform.Error

--import Uniform.HttpGet (makeHttpGet7, addPort2URI)
import Uniform.HttpCall (callHTTP10post, callHTTP8post
                , addPort2URI, makeHttpPost7, URI, HttpVarParams(..))
import Data.RDFext.Extension (ntFileTriples, sparqlConstructFile, turtleFile)
import LitTypes.TextDescriptor (serverBrest, rdfBase, dirQueries)
import Data.List.Split (chunksOf)
import Data.List.Utils (replace)
import Data.Either (isRight)
import CmdLineUtilities.UtilsProcessCmd
--import Process.UtilsParseArgs

ntExtension = Extension "nt"
--turtleExtension = Extension "ttl"  -- is typed file
turtleType =   Just "ttl"    -- for http call

oneConstruct2 :: Inputs ->  Path Abs File -> ErrIO Text
-- execute the construct query in fn against the fuseki store db in graph
-- db
-- timeout in sec (or nothing)
oneConstruct2 inp  fn0 = do
        putIOwords ["oneConstruct db - graph"
                    , "file", showT fn0
                    ]
        queryText :: [Text] <- read6  fn0 sparqlConstructFile
        -- cannot use the typedfile - would produce triples...
--        when debug $ putIOwords ["oneConstruct db -  \n",  unlines' queryText]

        -- needs to insert the graph in the query

        let query2 = unlines' queryText
        -- rdfBase is http://gerastree.at" defined in litFoundation
        -- works only if the graphs are produced with ntstore? TODO
        let graphDescription = maybe ""  -- default graph - working?
                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
                (inGraph inp) :: Text
--        let auxgraphDescription = maybe ""
--                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
--                maux :: Text

        -- the replacement should not do anything if no targets are found !

        let query4 = s2t . replace "#_graphSource" (t2s graphDescription)
                        . t2s $ query2
--        let query4 = s2t . replace "#_auxgraphSource" (t2s auxgraphDescription)
--                    . t2s $ query3
        let pathName = (inDB inp)  </> "sparql" -- "query"

--        when debug $ putIOwords ["oneConstruct query processed \n",  query4,
--                    "\npathName", pathName  ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be

        -- construct the call
        let fusekiServer = getServer  (inServer inp)
--        resp <- makeHttpPost7 debug server2 pathName
        let query = HttpVarParams  [ ("output", turtleType)]

        let appType = "application/sparql-query"

        resp <- callHTTP10post (inDebug inp) appType  fusekiServer  pathName
                    (b2bl . t2b $ query4) query  (inTimeOut inp)

--        let resp2 =   resp
--        let resultExt = makeExtension . t2s $ ( db <.>  "csv") :: Extension
        let respRoot = makeAbsDir $ getParentDir fn0 :: Path Abs Dir
                    -- the dir where the query is
        let queryName = getNakedFileName fn0 -- the query name
        let graphDesc = t2s $ (inDB inp)
                    <-> (maybe "" id (inGraph inp)) :: FilePath
        let respDir = addDir respRoot graphDesc
        let respFilename =     addFileName  respDir queryName :: Path Abs File

        putIOwords ["oneConstruct response\n" -- ,  resp
                , "for", showT fn0, "to file", showT respFilename]
        -- write the response into the flag file
--        writeFileOrCreate2   respFilename resp
        write6 respFilename turtleFile resp
        return resp
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["oneConstruct error caught axx343k",  e
                , "\n filename is", showT fn0
                , "error is not raised again"]
        putIOwords ["oneConstruct arguments were db - graph"
--                , db, showT mgraph
                , "file", showT fn0]
        return . unwords' $ ["oneConstruct return after error", showT e]
--        fail . unwords $  [ "callHTTP8post httperror 3", show e]



testfn0 = addFileName (dirQueries :: Path Abs Dir)
                             (t2s (("test" :: Text) </> ("test1" :: Text))  )
                                :: Path Abs File

--test_1 = do
--    r <- runErr $ oneConstruct True serverBrest "testDB"
--                (Just "t1")  Nothing testfn0
--    assertBool (isRight r)

