-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  Parse the cmd line for ntmake
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and converts markup to nt
-- takes   cmd line argument: -s gives source dir fuer die markup files.
-- later : -f for the filename
-- later: language (to process the english files first and then the other languages
-- in turn
-- merge with the parse of cmd line arg for store etc.

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdLineUtilities.CmdLineNtmake
        (NtmakeArgs (..), getArgsParsedNT
--            module CmdLineUtilities.UtilsParseArgs
--        , dirQueries, URI
    , module GHC.Generics
    , module Uniform.Error
--    , module Uniform.Zero


       ) where

import           Uniform.Error hiding ((<>), (</>), (<.>))
--import Uniform.Zero
import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative
import GHC.Generics


getArgsParsedNT :: Text -> Text -> ErrIO NtmakeArgs
getArgsParsedNT t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> ntcmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

-- cmd line parsing -- cannot use utilsparse, because very different required fields
data NtmakeArgs = NtmakeArgs
  {
     argLocalhost :: Bool -- ^ use localhost as fuseki server
     , argDestination :: String -- ^ the directory for the nt files
   , argOrigin :: String -- ^ the directoy in which the markup files are
   , argForceFlag :: Bool -- ^ force processing, even if newer exist
   , argFrenchUDFlag :: Bool -- ^ used UD model for french
   , argDebug :: Bool -- ^ controls debug output
   , argXML :: Bool -- ^ controls output of xml as text
  } deriving (Show)

ntcmdArgs :: Parser (NtmakeArgs)
ntcmdArgs = NtmakeArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
--            metavar "orig/test" <>
            help "localhost not serverBrest (default)" )
     <*> strOption
          ( long "destinationDir" <>
            short 'd' <>
            value "NT" <>
            metavar "Destination directory"
            <> help "directory for the nt files " )
     <*>  strOption
        (long "origin dir" <>
            short 'o'  <>
            value "" <>
            help "dir in which the markup or query files are (relative to home)" )
     <*> switch
          ( long "frenchUD" <>
            help "use for french the model trained for UD" )
     <*> switch
          ( long "XML" <>
            help "print xml" )

--convertFlags2list :: NtmakeArgs -> LitTextFlags
---- convert the flags to a list of LitTextFlag values
--convertFlags2list _ = []
--
--flag2bool :: [(NtmakeArgs -> Bool)] -> NtmakeArgs -> [Bool]
--flag2bool fs args = map (\f -> f args) fs
--
--args2flags :: NtmakeArgs -> LitTextFlags
---- find the flags set to true and put into list of flags
--args2flags args = selFlag [LocalNLPserverFlag, ForceFlag, DebugFlag, XMLflag]
--                $ flag2bool [argLocalhost, argForceFlag, argDebug, argXML] args
--
--selFlag :: [LitTextFlag] -> [Bool] -> [LitTextFlag]
--selFlag [] _  = []
--selFlag _ []  = []
--selFlag (a:as) (b:bs)  = if b then a : (selFlag as bs)  else selFlag as bs
--
--
--
--isMarkup :: Path Abs File -> Bool
--isMarkup  = hasExtension (Extension "markup")
---- todo include in typedfiles - hasType ...

