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


module LitTypes.UtilsProcessing
    (module LitTypes.UtilsProcessing
--        , module LitTypes.UtilsParseArgs
        , getTimeout
        , dirQueries, URI

--    , URI, serverBrest, serverLocalhost
    )
    where

--import           Test.Framework
import           Uniform.FileIO as FN hiding ((<>), (</>), (<.>))
import LitTypes.UtilsParseArgs
import LitTypes.ServerNames -- (serverLocalhost, serverBrest
--                    , rdfBase, dirQueries, URI, HttpVarParams)
import Uniform.HttpCall (callHTTP10post)
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
processAll ops testFile dir file = do  -- debug forceFlag server dir db mgraph  file =  do
    putIOwords ["processAllNTGZ", "dir", showT dir, "file", showT file]
--            db - graph", db, showT mgraph, "debug", showT debug]
    let path = toFilePath dir
    resFile :: Path Abs File <- makeAbsoluteFile' file
    bracketErrIO (FN.openFile2handle resFile WriteMode)
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


getServer server  = addPort2URI  server  3030 :: URI


post2store ::  Bool -> Text -> URI -> Text -> Maybe Text ->  LazyByteString
            -> HttpVarParams -> Maybe Int ->   ErrIO Text
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