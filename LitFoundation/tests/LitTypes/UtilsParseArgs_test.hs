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


module LitTypes.UtilsParseArgs_test  where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
--import           Uniform.Strings
--import           Uniform.Error

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative

import LitTypes.ServerNames (serverLocalhost, serverBrest, rdfBase, dirQueries, URI)
import Uniform.HttpCall (callHTTP8post, addPort2URI, callHTTP10post, URI)
import LitTypes.UtilsParseArgs


