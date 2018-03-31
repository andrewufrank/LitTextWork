-----------------------------------------------------------------------------
--
-- Module      :  Foundation - servers
-- Copyright   :  andrew u frank -
--
-- | the addresses of the servers
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
        , DeriveGeneric #-}

module LitTypes.Flags (
    module LitTypes.Flags
    ) where


import Uniform.FileIO (makeAbsDir, makeRelDir)
import Uniform.HttpURI -- hiding ((</>), (<.>))
import Data.RDFext.Extension (PartURI (..), unPartURI)
import GHC.Generics

data LitTextFlag = DebugFlag | ForceFlag | IncludeTextFlag
            | OutputNLPflag
--            | XMLflag
            | JSONflag
            | LocalNLPserverFlag
            | SnipSet Int
            | NoFlagZero
            deriving (Show, Read, Eq, Ord, Generic)

type LitTextFlags = [LitTextFlag]
instance Zeros LitTextFlag where zero = NoFlagZero


