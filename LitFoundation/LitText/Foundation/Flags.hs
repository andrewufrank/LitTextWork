-----------------------------------------------------------------------------
--
-- Module      :  Foundation - servers
-- Copyright   :  andrew u frank -
--
-- | the addresses of the servers
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}

{-# LANGUAGE ScopedTypeVariables
        , DeriveGeneric
        , DeriveAnyClass #-}

module LitText.Foundation.Flags (
--    module LitText.Foundation.Flags
--        args2flags,
      setFlags
       , LitTextFlag (..)
       , LitTextFlagSet  -- no access to internals
       , LitTextFlags (..)   -- all ops
    ) where


import Uniform.FileIO (makeAbsDir, makeRelDir, Zeros (..))
-- import Uniform.HttpURI -- hiding ((</>), (<.>))
import GHC.Generics

data LitTextFlag = DebugFlag | ForceFlag | IncludeTextFlag
            | OutputNLPflag
--            | XMLflag
            | JSONflag
            | LocalNLPserverFlag
            | SnipSet Int
            | NoFlagZero
            deriving (Show, Read, Eq, Ord, Generic)

newtype LitTextFlagSet = LitTextFlagSet [LitTextFlag]
            deriving (Show, Read, Eq, Ord, Generic, Zeros)
instance Zeros LitTextFlag where zero = NoFlagZero

class LitTextFlags f where
    isFlagSet :: LitTextFlag -> f -> Bool
    isDebugFlag ::  f -> Bool
    isDebugFlag = isFlagSet DebugFlag
    isIncludeText :: f -> Bool
    isIncludeText = isFlagSet IncludeTextFlag
    isForceFlag :: f -> Bool
    isForceFlag = isFlagSet ForceFlag
    isLocalServer :: f -> Bool
    isLocalServer = isFlagSet LocalNLPserverFlag




instance LitTextFlags LitTextFlagSet where
    isFlagSet d (LitTextFlagSet fs) = d `elem` fs
--    isDebugFlag (LitTextFlagSet fs) =
--        let nlpserver = if LocalNLPserverFlag `elem` flags
--                    then serverLocalhost
--                    else serverBrest


--args2flags :: LitArgs -> LitTextFlags
---- find the flags set to true and put into list of flags
--args2flags args = setFlags [LocalNLPserverFlag, ForceFlag, DebugFlag]
--                $ flag2bool [argLocalhost, argForceFlag, argDebug] args

setFlags :: [LitTextFlag] -> [Bool] -> LitTextFlagSet
setFlags fs bs  = LitTextFlagSet $ setFlags_ fs bs

setFlags_ :: [LitTextFlag] -> [Bool] -> [LitTextFlag]
setFlags_ [] _  = []
setFlags_ _ []  = []
setFlags_ (a:as) (b:bs)  = if b then a : (setFlags_ as bs)  else setFlags_ as bs
