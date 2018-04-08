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

module LitText.Foundation (
        module LitText.Foundation.TextDescriptor
    , module LitText.Foundation.ServerNames
    , module LitText.Foundation.LanguageTypedText
--    , module Uniform.Strings  -- cannot export FileIO as well
    , module Uniform.FileIO
    , module Uniform.Error
    , module Data.RDFext
    , LanguageCode (..) -- from rdf4hextension
    , NTdescriptor (..)
--    , LitTextFlagSet
    ,  LitTextFlag (..), LitTextFlags (..), setFlags
--      export the individual flag names (at least used in UtilsProcessCmd)
    , Zeilen (..), BuchToken (..), MarkupElement (..)
    , Unparser(..), combine2linesWithHyphenation
    , Zeros (..)
    ) where

import Uniform.FileIO hiding ((<>), (</>), (<.>))
--import Uniform.Strings  -- gives the <> etc.
import Uniform.Error hiding ((<>), (</>), (<.>))
import LitText.Foundation.ServerNames  -- (URI, makeURI, serverBrest)  -- for test
import LitText.Foundation.BuchToken
import LitText.Foundation.LanguageTypedText
import LitText.Foundation.Flags
import LitText.Foundation.Classes4text hiding ((<>), (</>), (<.>))
import LitText.Foundation.BuchToken hiding ((<>), (</>), (<.>))
import LitText.Foundation.TextDescriptor hiding ((<>), (</>), (<.>))
import Data.RDFext

