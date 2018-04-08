-----------------------------------------------------------------------------
--
-- Module      :  LitText.Foundation . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances
    , DeriveGeneric
    , RecordWildCards
    , DeriveAnyClass #-}

module LitText.CmdLineUtilities  (
            processAll
            , getArgsParsed
--            , setDefaultOriginDir, selectServer
--            , LitTextFlag (..)
            , LitTextFlags
            , parseAndStartExecute
            , Inputs(..)
            , addFusekiPort
        )  where

import LitText.CmdLineUtilities.UtilsParseArgs
import LitText.CmdLineUtilities.UtilsProcessCmd
import LitText.CmdLineUtilities.UtilsProcessing


