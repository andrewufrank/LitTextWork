 -----------------------------------------------------------------------------
--
-- Module      :  utils
-- Copyright   :  andrew u frank -
--
-- | utilities for the query and storage processing
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Process.UtilsProcessing
    (module Process.UtilsProcessing
--        , module Process.UtilsParseArgs
        , getTimeout
        , dirQueries, URI

--    , URI, serverBrest, serverLocalhost
    )
    where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import Process.UtilsParseArgs
import Producer.Servers (serverLocalhost, serverBrest, rdfBase, dirQueries, URI)
import Uniform.HttpCallWithConduit (callHTTP8post, addPort2URI, callHTTP10post, URI, HttpQueryString)
import           Uniform.Strings


import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
-- todo fileio - export for pipes

processAll :: (Path Abs File -> ErrIO Text) -> (Path Abs File -> Bool)
        -> Path Abs Dir -> Path ar File  -> ErrIO Text
-- | get all   files passing the test from the dir and process them with the ops
-- the file is the result report, not informativ
processAll ops testFile dir file = do  -- debug forceFlag server dir db mgraph  file =  do
    putIOwords ["processAllNTGZ", "dir", showT dir, "file", showT file]
--            db - graph", db, showT mgraph, "debug", showT debug]
    let path = toFilePath dir
    resFile <- makeAbsolute file
    bracketErrIO (openFile2handle resFile WriteMode)
                closeFile2  -- not with transaction tmp
                (\hand ->
                      Pipe.runEffect $
                        getRecursiveContents dir
                        >-> Pipe.filter testFile --
                        >-> Pipe.mapM (fmap t2s . ops) --  putOneFile2xx debug forceFlag server db mgraph)
                    --    >-> P.stdoutLn
                        >-> Pipe.toHandle hand
                )
    return $ unwords' ["processAll", showT dir, showT file , "ok"]

--putOneFile2xx :: Bool -> Bool -> URI -> Text -> Maybe Text -> Path Abs File -> ErrIO Text
---- put one file into the db and graph
---- checks for the flag to avoid duplication
---- but does separately check (and possibly duplicate) an nt and a nt.gz file
--
--putOneFile2xx debug forceFlag server db mgraph fn = return ""

getServer server  = addPort2URI  server  3030 :: URI


post2store ::  Bool -> Text -> URI -> Text -> Maybe Text ->  LazyByteString
            -> HttpQueryString -> Maybe Int ->   ErrIO Text
    -- ^ timeout in sec

-- use post with multipart form to store (see https://www.w3.org/TR/sparql11-http-rdf-update/#http-post)
post2store debug appType fusekiServer pathName mgraph split qstring timeout  = do
    -- form the instert query
    let pathNamePlusGraph = pathName <> "?" <>
             maybe "default"  (\t -> "graph=" <> t)
                    mgraph
--    when debug $
--    putIOwords ["\npost2store for",  fn, "path", pathNamePlusGraph ]
    putIOwords ["post2store", "pathNamePlusGraph",   pathNamePlusGraph, "qstring", showT qstring]

    res <- callHTTP10post True appType
                fusekiServer pathNamePlusGraph
                split qstring   timeout
    when True $ putIOwords ["post2store done", showT res]
    return res

--        D
