-----------------------------------------------------------------------------
--
-- Module      :  LitText.Foundation . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the additional exports for testing
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances
    , DeriveGeneric
    , RecordWildCards
    , DeriveAnyClass #-}

module LitText.Lines.Lines (
    TextZeile(..)
    , parseMarkup
    ) where

import Uniform.FileIO hiding ((<>), (</>), (<.>))
--import Uniform.Strings  -- gives the <> etc.
import Uniform.Error hiding ((<>), (</>), (<.>))
import Data.RDFext
import LitText.Lines.HandleLayout
import LitText.Lines.Lines2para
import LitText.Lines.Lines2text
import LitText.Lines.MarkupText


