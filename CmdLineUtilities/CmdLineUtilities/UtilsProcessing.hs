 -----------------------------------------------------------------------------
--
-- Module      :  utils
-- Copyright   :  andrew u frank -
--
-- | utilities for the query and storage processing
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module CmdLineUtilities.UtilsProcessing
    (module CmdLineUtilities.UtilsProcessing
--        , module CmdLineUtilities.UtilsParseArgs
--        , getTimeout
--        , dirQueries,
       , URI, HttpVarParams
    )
    where

import           Uniform.FileIO as FN hiding ((<>), (</>), (<.>))
import Uniform.Strings  ((<>))


import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
--import qualified Path.IO as Path.IO
-- todo fileio - export for pipes

processAll :: (Path Abs File -> ErrIO Text) -> (Path Abs File -> Bool)
        -> Path Abs Dir -> Path ar File  -> ErrIO Text
-- | get all   files passing the test from the dir and process them with the ops
-- the file is the result report, not informativ
processAll ops testFile dir file = do
        -- debug forceFlag server dir db mgraph  file =  do
    putIOwords ["processAll", "dir", showT dir, "file", showT file]
--            db - graph", db, showT mgraph, "debug", showT debug]
    let path = toFilePath dir
    resFile :: Path Abs File <- makeAbsoluteFile' file
    bracketErrIO (FN.openFile2handle resFile WriteMode)
                (\hand -> do
                    putIOwords ["processAll close"]
                    closeFile2 hand -- not with transaction tmp
                    )
                (\hand ->
                      Pipe.runEffect $
                        getRecursiveContents dir
                        >-> Pipe.filter testFile --
                        >-> Pipe.mapM (fmap t2s . ops)
                            --  putOneFile2xx debug forceFlag server db mgraph)
                    --    >-> P.stdoutLn
                        >-> Pipe.toHandle hand
                )
    return $ unwords' ["processAll end", showT dir, showT file , "ok"]


addFusekiPort server  = addPort2URI  server  3030 :: URI


post2store ::  Bool -> Text -> URI -> PartURI
                -> Maybe PartURI ->  LazyByteString
            -> HttpVarParams -> Maybe Int ->   ErrIO Text
    -- ^ timeout in sec

-- use post with multipart form to store
--(see https://www.w3.org/TR/sparql11-http-rdf-update/#http-post)
post2store debug appType fusekiServer pathName
                mgraph payload varparms timeout = do
    -- form the instert query
    let pathNamePlusGraph = (unPartURI pathName) <> "?" <>
             maybe "default"  (\t -> "graph=" <> (unPartURI t))
                    mgraph
--    when debug $
--    putIOwords ["\npost2store for",  fn, "path", pathNamePlusGraph ]
    putIOwords ["post2store", "pathNamePlusGraph"
            ,   pathNamePlusGraph, "qstring", showT varparms]
    res <- callHTTP10post debug appType
                (addFusekiPort fusekiServer) pathNamePlusGraph
                payload varparms   (TimeOutSec timeout)
    when True $ putIOwords ["post2store done", showT res]
    return res

--
