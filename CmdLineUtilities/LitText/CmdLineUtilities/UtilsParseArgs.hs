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


module LitText.CmdLineUtilities.UtilsParseArgs
    (LitArgs (..)
        , getArgsParsed, flag2bool, getTimeout
--            module CmdLineUtilities.UtilsParseArgs
--        , dirQueries, URI
    , module GHC.Generics
    , module Uniform.Error
--    , module Uniform.Zero
    )
    where

--import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Error hiding ((<>), (</>), (<.>))
--import Uniform.Zero
import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative
import GHC.Generics
import Uniform.Http hiding ((<>), (</>), (<.>))
-- import Uniform.HttpURI hiding ((<>), (</>), (<.>))  -- remove later

getTimeout :: LitArgs -> TimeOutSec
getTimeout args = maybe (mkTimeOutDefault) (mkTimeOut . (60 *)) t1
    where
        t1 = readMay (argTimeout args)  :: Maybe Int

--- cmd line parsing
data LitArgs = LitArgs
  {
    argLocalhost :: Bool -- ^ l - use localhost as fuseki server
  , argDB :: String -- ^ b - the database name
  , argGraph  :: String  -- ^ g - the graph
   , argOrigin :: String -- ^ o - the directoy in which the markup
--                            or query files are
   , argDestination :: String -- ^ d - the directory for the nt or result files

  , argFn  :: String -- ^ f - the filename without extension - if any
                -- relative to origin
  , argTimeout  :: String -- ^ the timeout (if any) in minutes
  , argDebug :: Bool -- ^ controls debug output
  , argForceFlag :: Bool -- ^ force processing, even if newer exist
   , argFrenchUDFlag :: Bool -- ^ used UD model for french
--  , argResultDir :: String -- ^ the result dir
--  , argWordnetGraph  :: String -- ^ the name of the wordnet graph without <..>
--  , argSubDir  :: String  -- ^ the query dir
--  , argAuxGraph  :: String  -- ^ an additional graph
--  , argDest  :: String  -- ^ the destination graph
--  , argQuery :: Bool -- ^ query to produce file, not an update
--  , argBookNrFile :: String -- ^ a single book (not a full link)

  } deriving (Show)


flag2bool :: [(LitArgs -> Bool)] -> LitArgs -> [Bool]
flag2bool fs args = map (\f -> f args) fs

cmdArgs :: Parser LitArgs
cmdArgs = LitArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
            help "localhost not serverBrest (default)" )
     <*> strOption
          ( long "(data)base (was corpus)" <>
            short 'b' <>
        --   long "subdir" <>
            value "" <>
            metavar "base - dataset - required for store, queries etc." <>
            help "dbname from fuseki (was corpus)" )
     <*> strOption
          ( long "graph - required" <>
            short 'g' <>
            metavar "Graph" <>
            value "" <>
            help "graph - - required for store, queries etc." )
     <*>  strOption
        (long "origin dir" <>
            short 'o'  <>
--            value "" <>  -- at least one value must be given
            help "dir in which the markup or query files are (relative to home)" )
     <*> strOption
          ( long "destinationDir" <>
            short 'd' <>
            value "" <>
            metavar "Destination directory"
            <> help "directory for the nt files " )
     <*> strOption
          ( long "filename of construct query" <>
            short 'f' <>
            value "" <>
            metavar "query (filename)" <>
            help "filename (without .construct), relative to origin, \
                            \will also be name of resulting .ttl" )
     <*> strOption
          ( long "timeout" <>
            short 't' <>
            value "" <>
            metavar "timeout" <>
            help "timeout in minutes " )
     <*> switch
          ( long "debug" <>
            help "put debug output" )
     <*> switch
          ( long "force" <>
            help "force processing even when newer exist (default false)" )
     <*> switch
          ( long "frenchUD" <>
            help "use for french the model trained for UD" )
--     <*> strOption
--          ( long "auxGraph" <>
--            short 'a' <>
--            value "" <>
--            metavar "Auxilliary Graph" <>
--            help "graph" )
--     <*> strOption
--          ( long "dest" <>
--            short 'd' <>
--            value "" <>
--            metavar "Destination Graph" <>
--            help "destination graph" )
--     <*> strOption
--          ( long "subdir for queries or nt" <>
--            short 's' <>
--            value "" <>
--            metavar "subdir (folder) with the construct queries" <>
--            help "subdir (folder) with the construct queries to execute" )
--     <*> strOption
--          ( long "result dir" <>
--            short 'r' <>
--            value "/home/frank/constructResults" <>
--            metavar "result dir" <>
--            help "filename (without .construct), will also be name of resulting .ttl" )
--     <*> strOption
--          ( long "wordnet" <>
--            short 'w' <>
--            value "" <> -- "http://nlp.gerastree.at:3030/corpus3/data/wn31" <>
--            metavar "wordnet graph" <>
--            help "the wordnet graph without <..> " )
--     <*>  strOption
--        (long "origin dir (relative to home)" <>
--            short 'o'  <>
--            value "" <>
--            help "dir in which the nt?, markup or query files are \
--                        \(relative to home)" )
--     <*>  strOption
--        (long "input file with the booknumbers " <>   -- must be synced manually to brest?
--            short 'i'  <>
--            value "" <> -- set to in main /home/frank/gutenberg" <>
--            help "input file with the booknumbers - cvs, booknr in first column" )

--setDefaultOriginDir :: LitArgs -> String -> LitArgs
---- is always homedir
--setDefaultOriginDir args def =
--        if null' $ argOrigin args
--            then  args {argOrigin = def}
--            else args

getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

--
