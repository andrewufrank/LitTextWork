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

module LitText.Lines (
    Zeilen (..)
    , module Uniform.FileIO
    , module Uniform.Error
    , module Data.RDFext
    , LanguageCode (..) -- from rdf4hextension
    , NTdescriptor (..)
    , LitTextFlagSet,  LitTextFlag (..), LitTextFlags (..), setFlags
    -- export the individual flag names (at least used in UtilsProcessCmd)
    , Zeros (..)
    ) where

import Uniform.FileIO hiding ((<>), (</>), (<.>))
--import Uniform.Strings  -- gives the <> etc.
import Uniform.Error hiding ((<>), (</>), (<.>))
import Data.RDFext
import LitText.Lines.HandleLayout
import LitText.Lines.Lines2para
import LitText.Lines.Lines2text
import LitText.Lines.MarkupText


