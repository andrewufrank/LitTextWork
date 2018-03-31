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

module CmdLineUtilities.ProcessFlags
    (LitTextFlag (..), LitTextFlags (..)
    , args2flags
    ) where

--import           Uniform.Error hiding ((<>), (</>), (<.>))
import Uniform.Zero
--import           Data.Semigroup               ((<>))
--import           Options.Applicative.Builder
--import           Options.Applicative
import GHC.Generics
import CmdLineUtilities.UtilsParseArgs
import LitTypes.Flags

--data LitTextFlag = DebugFlag | ForceFlag | IncludeTextFlag
--            | OutputNLPflag
----            | XMLflag
--            | JSONflag
--            | LocalNLPserverFlag
--            | SnipSet Int
--            | NoFlagZero
--            deriving (Show, Read, Eq, Ord, Generic)
--
--type LitTextFlags = [LitTextFlag]
--instance Zeros LitTextFlag where zero = NoFlagZero

convertFlags2list :: f -> LitTextFlags
-- convert the flags to a list of LitTextFlag values
convertFlags2list _ = []

flag2bool :: [(f -> Bool)] -> f -> [Bool]
flag2bool fs args = map (\f -> f args) fs

args2flags :: LitArgs -> LitTextFlags
-- find the flags set to true and put into list of flags
args2flags args = selFlag [LocalNLPserverFlag, ForceFlag, DebugFlag]
                $ flag2bool [argLocalhost, argForceFlag, argDebug] args

selFlag :: [LitTextFlag] -> [Bool] -> [LitTextFlag]
selFlag [] _  = []
selFlag _ []  = []
selFlag (a:as) (b:bs)  = if b then a : (selFlag as bs)  else selFlag as bs



--isMarkup :: Path Abs File -> Bool
--isMarkup  = hasExtension (Extension "markup")
---- todo include in typedfiles - hasType ...

