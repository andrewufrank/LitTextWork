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
{-# LANGUAGE ScopedTypeVariables
    , RecordWildCards  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module LitText.Process.OneConstruct (module LitText.Process.OneConstruct
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Strings
import           Uniform.Error
import LitText.Foundation
--        (serverBrest, serverLocalhost
--            , rdfBase, dirQueries, PartURI, unPartURI
--           , getServer, LitTextFlags (..))
import Data.List.Split (chunksOf)
import Data.List.Utils (replace)
import Data.Either (isRight)
import LitText.CmdLineUtilities

ntExtension = Extension "nt"
--turtleExtension = Extension "ttl"  -- is typed file
turtleType =   Just "ttl"    -- for http call

oneConstruct2 :: Inputs ->  Path Abs File -> ErrIO Text
-- execute the construct query in fn against the fuseki store db in graph
-- db
-- timeout in sec (or nothing)
oneConstruct2 inp@Inputs{..}  fn0 = do
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
                (inGraph ) :: Text
--        let auxgraphDescription = maybe ""
--                (\a -> concat' [" <", showT rdfBase, "/", a, ">"])
--                maux :: Text

        -- the replacement should not do anything if no targets are found !

        let query4 = s2t . replace "#_graphSource" (t2s graphDescription)
                        . t2s $ query2
--        let query4 = s2t . replace "#_auxgraphSource" (t2s auxgraphDescription)
--                    . t2s $ query3
        let pathName = mkHttpPath . unIRI $
                            append2IRIwithSlash (toIRI inDB) "sparql"
                               :: HttpPath   -- "query"

--        when debug $ putIOwords ["oneConstruct query processed \n",  query4,
--                    "\npathName", pathName  ]

--        let graphName = maybe ("", Nothing)
--                (\a -> ("named-graph-uri", Just $ showT rdfBase </> a) )
--                mgraph
        -- not clear what the names of the graphs would be

        -- construct the call
        let fusekiServer = getServer inp

        let query = mkHttpQueryParams [ ("output", turtleType)]

        let appType = mkAppType "application/sparql-query"

        resp <- callHTTP10post (isDebugFlag inp) appType
                    (addFusekiPort fusekiServer)  pathName
                    (b2bl . t2b $ query4)
                    query  (inTimeOut)

--        let resp2 =   resp
--        let resultExt = makeExtension . t2s $ ( db <.>  "csv") :: Extension
        let respRoot = makeAbsDir $ getParentDir fn0 :: Path Abs Dir
                    -- the dir where the query is
        let queryName = getNakedFileName fn0 -- the query name
        let graphDesc = makeRelDir . t2s .   unIRI $ append2IRI (toIRI inDB )
                   (maybe "" (""<->) inGraph)
                   :: Path Rel Dir
        let respDir = addFileName respRoot graphDesc :: Path Abs Dir
        -- falscher funct name ...
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



--testfn0 = addFileName (dirQueries :: Path Abs Dir)
--                             (t2s (("test" :: Text) </> ("test1" :: Text))  )
--                                :: Path Abs File

--test_1 = do
--    r <- runErr $ oneConstruct True serverBrest "testDB"
--                (Just "t1")  Nothing testfn0
--    assertBool (isRight r)

