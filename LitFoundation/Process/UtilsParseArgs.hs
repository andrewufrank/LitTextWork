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


module Process.UtilsParseArgs
    (module Process.UtilsParseArgs
        , dirQueries, URI

--    , URI, serverBrest, serverLocalhost
    )
    where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
--import           Uniform.Strings
--import           Uniform.Error

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative

import Producer.Servers (serverLocalhost, serverBrest, rdfBase, dirQueries, URI)
import Uniform.HttpCallWithConduit (callHTTP8post, addPort2URI, callHTTP10post, URI)

-- check all filenames by converting to Path format

--- cmd line parsing
data LitArgs = LitArgs
  {
--    argOrigTest   :: Bool   -- ^ orig or test to decide where to take the file
    argLocalhost :: Bool -- ^ use localhost as fuseki server
--  , argQuery :: Bool -- ^ query to produce file, not an update
  , argDB :: String -- ^ the database name
  , argGraph  :: String  -- ^ the graph
  , argAuxGraph  :: String  -- ^ an additional graph
--  , argDest  :: String  -- ^ the destination graph
  , argSubDir  :: String  -- ^ the query dir
  , argFn  :: String -- ^ the query filename without extension
--  , argResultDir :: String -- ^ the result dir
  , argWordnetGraph  :: String -- ^ the name of the wordnet graph without <..>
  , argTimeout  :: String -- ^ the timeout (if any) in minutes
   , argOrigin :: String -- ^ the directoy in which the markup files are
   , argForceFlag :: Bool -- ^ force processing, even if newer exist
    , argBookNrFile :: String -- ^ a single book (not a full link)

  } deriving (Show)

cmdArgs :: Parser LitArgs
cmdArgs = LitArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
--            metavar "orig/test" <>
            help "localhost not serverBrest (default)" )
--     <*> switch
--          ( long "query" <>
--            short 'q' <>
----            metavar "orig/test" <>
--            help "query (not update)" )
     <*> strOption
          ( long "database (was corpus)" <>
            short 'd' <>
        --   long "subdir" <>
            metavar "db (was corpus) - required " <>
            help "dbname from fuseki (was corpus)" )
     <*> strOption
          ( long "graph - required" <>
            short 'g' <>
            metavar "Graph" <>
            help "graph - required" )
     <*> strOption
          ( long "auxGraph" <>
            short 'a' <>
            value "" <>
            metavar "Auxilliary Graph" <>
            help "graph" )
--     <*> strOption
--          ( long "dest" <>
--            short 'd' <>
--            value "" <>
--            metavar "Destination Graph" <>
--            help "destination graph" )
     <*> strOption
          ( long "subdir" <>
            short 's' <>
            value "" <>
            metavar "subdir (folder) with the construct queries" <>
            help "subdir (folder) with the construct queries to execute" )
     <*> strOption
          ( long "filename of construct query" <>
            short 'f' <>
            value "" <>
            metavar "query (filename)" <>
            help "filename (without .construct), will also be name of resulting .ttl" )
--     <*> strOption
--          ( long "result dir" <>
--            short 'r' <>
--            value "/home/frank/constructResults" <>
--            metavar "result dir" <>
--            help "filename (without .construct), will also be name of resulting .ttl" )
     <*> strOption
          ( long "wordnet" <>
            short 'w' <>
            value "" <> -- "http://nlp.gerastree.at:3030/corpus3/data/wn31" <>
            metavar "wordnet graph" <>
            help "the wordnet graph without <..> " )
     <*> strOption
          ( long "timeout" <>
            short 't' <>
            value "" <>
            metavar "timeout" <>
            help "timeout in minutes " )
     <*>  strOption
        (long "origin dir" <>
            short 'o'  <>
            value "" <>
            help "dir in which the nt?, markup or query files are (relative to home)" )
     <*> switch
          ( long "force" <>
--            value False <>
--            short 'l' <>
--            metavar "force processing " <>
            help "force processing even when newer exist (default false)" )
     <*>  strOption
        (long "input file with the booknumbers " <>   -- must be synced manually to brest?
            short 'i'  <>
            value "" <> -- set to in main /home/frank/gutenberg" <>
            help "input file with the booknumbers - cvs, booknr in first column" )

setDefaultOriginDir :: LitArgs -> String -> LitArgs
setDefaultOriginDir args def = if null' $ argOrigin args then  args {argOrigin = def}
                                                else args

getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

selectServer :: LitArgs -> URI
-- select the server between localhost and brest
selectServer args =  if  argLocalhost args
                    then serverLocalhost
                    else serverBrest  -- default

--getLocalOriginDir
-- produces the abs dir combined from home origin and subfile dir
getLocalOriginDir args = if null' subdir then orig
                                        else addDir  orig subdir :: Path Abs Dir
                where subdir = argSubDir args
                      orig = addDir homeDir $  argOrigin args

getFn args = addFileName (getLocalOriginDir args) (argFn args) :: Path Abs File
-- get filename (added to the local origin)
-- may fail or

--test_queryDir = assertEqual "" (addDir

getTimeout :: LitArgs -> Maybe Int
getTimeout args = fmap (60 *) t1
    where
--        t1 = readMay ("30"::String) :: Maybe Int
        t1 = readMay (argTimeout args)  :: Maybe Int


--        D
