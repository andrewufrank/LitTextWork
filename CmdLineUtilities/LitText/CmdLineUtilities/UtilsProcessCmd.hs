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


module LitText.CmdLineUtilities.UtilsProcessCmd
    (
--        module CmdLineUtilities.UtilsProcessCmd
--    , LitTextFlag (..), LitTextFlags (..)
      ErrIO, Inputs (..)
    , LitTextFlags (..) -- class
--    , PartURI (..)
--    , addFusekiPort
--        , dirQueries
    )
    where

import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import LitText.CmdLineUtilities.UtilsParseArgs -- (getArgsParsed, getTimeout)
import LitText.CmdLineUtilities.UtilsProcessing (addFusekiPort)
import LitText.Foundation
import Uniform.Http --  (TimeOutSec)
--import Data.RDFext

data Inputs = Inputs {
         inDB :: RDFdataset  -- ^ the db to use
        , inGraph :: Maybe Text -- ^ the graph
        , inOriginDir :: Path Abs Dir -- ^ where the in files are
        , inDestinationDir :: Path Abs Dir -- ^ where the produced files go
        , inFilename :: Maybe (Path Rel File)
                    -- ^ a filename, if only one to process
                    -- ^ relative to OriginDir
        , inTimeOut :: TimeOutSec  -- ^ set a timeout for the process
        , inFlags :: LitTextFlagSet -- ^ all flags
        , inResultFile :: Path Abs File -- ^ file to keep result output
                    -- perhaps sufficient to return ?
--                    inServer :: ServerFlag  -- localhost
--        , inArgs :: LitArgs
--                    , inDebug :: Bool
--                    , inForceFlag :: Bool
                    } deriving (Show)

instance LitTextFlags Inputs where
    isFlagSet f  Inputs {..} = isFlagSet f inFlags

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
         originDir =  addDir homeDir (argOrigin args) :: Path Abs Dir
         destinationDir =  addDir homeDir (argDestination args) :: Path Abs Dir
            -- getLocalOriginDir args -- addDir homeDir (argOrigin args)
         fn = if null . argFn $ args
                then Nothing
                else Just . makeRelFile . argFn $ args
--                        . addFileName homeDir. argFn $ args
                                :: Maybe (Path Rel File)
    createDirIfMissing' destinationDir
    createDirIfMissing' (getParentDir resultFile)
    putIOwords ["parseAndStartExecute:"
            , " the arguments always necessary or optional with defaults "
--                , "\n\tserver", showT server
                , "\n\tdbarg (-b)", showT dbarg
                , "\n\tgraph (-g)", showT mgraph  -- optional for queries with all graphs
                , "\n\toriginDir (-o)", showT originDir
                , "\n\tfilename (-f)", showT fn
                , "\n\tdestinationDir (-d)", showT destinationDir
                , "\n\ttimeout (-t)", showT timeout
--                    , "\n\tqueryFile", showT fn
                ]
    let boolSwitches = flag2bool [argLocalhost, argForceFlag, argDebug] args
        flags = setFlags [LocalNLPserverFlag, ForceFlag, DebugFlag] boolSwitches
    putIOwords ["parseAndStartExecute flags", showT flags]
    let inp = Inputs {
          inDB = mkRDFdataset dbarg
        , inGraph = mgraph
        , inOriginDir = originDir
        , inDestinationDir = destinationDir
        , inFilename = fn
        , inTimeOut = timeout
        , inFlags = flags
        , inResultFile = resultFile
--                inArgs = args
--                , inDebug = debugFlag
--                , inForceFlag = forceFlag
--                , inServer = server
--                , inResultFile = resultFile
                }
    putIOwords ["parseAndStartExecute:  inputs ", showT inp]
    return inp


--getSwitches :: LitArgs -> [Bool]
--getSwitches args = flag2bool [argLocalhost, argForceFlag, argDebug] args

