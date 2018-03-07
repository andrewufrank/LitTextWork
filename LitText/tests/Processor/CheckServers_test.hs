-----------------------------------------------------------------------------
--
-- Module      :  CheckServers
-- Copyright   :  andrew u frank -
--
-- |  check whether the servers are working
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Processor.CheckServers_test
 where

import           Test.Framework

--import Uniform.HttpURI
import Uniform.HttpCall
import Parser.TextDescriptor
import LitTypes.ServerNames
import

test_checkAll = do
    r <-  (runErr checkAll)
    assertEqual (Right "") r
