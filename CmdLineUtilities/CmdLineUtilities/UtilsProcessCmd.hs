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
{-# LANGUAGE ScopedTypeVariables,
    DeriveGeneric, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module CmdLineUtilities.UtilsProcessCmd
    (module CmdLineUtilities.UtilsProcessCmd
    , LitTextFlag (..), LitTextFlags (..)
--        , dirQueries
    )
    where

import           Uniform.FileIO hiding ((<>), (</>), (<.>))
--import           Uniform.FileIO (homeDir2)
import CmdLineUtilities.UtilsParseArgs
import CmdLineUtilities.ProcessFlags

--import Uniform.Error hiding ((<>), (</>), (<.>))
--import           Data.Semigroup               ((<>))
--import           Options.Applicative.Builder
--import           Options.Applicative
--import GHC.Generics
--import LitTypes.TextDescriptor (serverLocalhost, serverBrest
--            , rdfBase, dirQueries, URI)
--import Path.IO as Pathio

--data LitTextFlag = DebugFlag | ForceFlag | IncludeTextFlag
--            | OutputNLPflag | XMLflag | JSONflag
--            | LocalNLPserverFlag
--            | SnipSet Int
--            | NoFlagZero
--            deriving (Show, Read, Eq, Ord, Generic)
--
--data ServerFlag = LocalServer | RemoteServer
--            deriving (Show, Read, Eq, Ord, Generic)




--selectServer :: LitArgs -> ServerFlag
---- select the server between localhost and brest
--selectServer args =  if  argLocalhost args
--                    then LocalServer
--                    else RemoteServer  -- default

----getLocalOriginDir
---- produces the abs dir combined from home origin and subfile dir
--getLocalOriginDir args = if null' subdir then orig
--                                  else addDir  orig subdir :: Path Abs Dir
--                where subdir = argSubDir args :: FilePath
--                      orig = addDir homeDir $  argOrigin args :: Path Abs Dir

--getFn args = addFileName (getLocalOriginDir args) (argFn args) :: Path Abs File
-- get filename (added to the local origin)
-- may fail or

getTimeout :: LitArgs -> Maybe Int
getTimeout args = fmap (60 *) t1
    where
--        t1 = readMay ("30"::String) :: Maybe Int
        t1 = readMay (argTimeout args)  :: Maybe Int

data Inputs = Inputs {
                     inDB :: Text
                    , inGraph :: Maybe Text
                    , inOriginDir :: Path Abs Dir
                    , inFilename :: Maybe (Path Abs File)
                    , inTimeOut :: Maybe Int
                    , inFlags :: LitTextFlags
--                    , inResultFile :: Path Abs File
--                    inServer :: ServerFlag  -- localhost
--        , inArgs :: LitArgs
--                    , inDebug :: Bool
--                    , inForceFlag :: Bool
                    } deriving (Show)

parseAndStartExecute :: Bool -> Text
                    -> Text -> Text -> ErrIO Inputs
-- the common operations to start the store, query and construct
-- arguments: rel dir (to home) of origin
-- name for result file
-- debug flag value
parseAndStartExecute debugFlag resultFileName  t1 t2 = do
    args1 <- getArgsParsed t1 t2
    homeDir :: Path Abs Dir <- homeDir2
    let args = args1
            -- setDefaultOriginDir args1 (toFilePath originDir)
    putIOwords ["parseAndStartExecute: process to store", showT args]
--    let server = selectServer args :: ServerFlag
    let resultFile = addFileName homeDir (t2s resultFileName ::FilePath)
                :: Path Abs File
    -- not really interesting inofrmation
--    let forceFlag = argForceFlag args
--    let debugFlag = True
--        putIOwords ["parseAndStartExecute: before making args "]
    let  timeout = getTimeout args
         mgraph = if null' . argGraph $ args then Nothing
                        else Just . s2t $  argGraph args  -- nothing if empty
--        putIOwords ["parseAndStartExecute: before making args 2"]
    let  dbarg = s2t $ argDB args
         originDir =  homeDir :: Path Abs Dir
            -- getLocalOriginDir args -- addDir homeDir (argOrigin args)
         fn = if null . argFn $ args
                then Nothing
                else Just . addFileName homeDir. argFn $ args :: Maybe (Path Abs File)
    putIOwords ["parseAndStartExecute:   the arguments always necessary or optional with defaults "
--                , "\n\tserver", showT server
                , "\n\tdbarg", showT dbarg
                , "\n\tgraph", showT mgraph  -- optional for queries with all graphs
                , "\n\toriginDir", showT originDir
                , "\n\ttimeout", showT timeout
--                    , "\n\tqueryFile", showT fn
                ]
    let flags = args2flags args
    putIOwords ["parseAndStartExecute flags", showT flags]
    let inp = Inputs {
                  inDB = dbarg
                , inGraph = mgraph
                , inOriginDir = originDir
                , inFilename = fn
                , inTimeOut = timeout
--                inArgs = args
--                , inDebug = debugFlag
--                , inForceFlag = forceFlag
--                , inServer = server
--                , inResultFile = resultFile
                }
    putIOwords ["parseAndStartExecute:  inputs ", showT inp]
    return inp


--
