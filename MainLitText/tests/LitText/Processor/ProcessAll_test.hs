 -----------------------------------------------------------------------------
--
-- Module      :  Processor ProcessAll
-- Copyright   :  andrew u frank -
--
-- | finds all markup files and process them to store in triple store
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module LitText.Process.ProcessAll_test  where

import           Test.Framework

import LitText.Parser.TextDescriptor hiding ((<>) , (</>), (<.>))
--import LitTypes.ServerNames
import LitText.Process.Main2sub
import LitText.Lines2para.Lines2ignore (LanguageCode(..)) -- hiding ((<>) , (</>), (<.>))

-- import CoreNLP.Snippets2nt as Snippets2nt (nlp_serverLoc, host_serverLoc)

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
import Uniform.FileIO
import LitTypes.UtilsParseArgs ( LitTextFlags (..) )
import LitText.Process.ProcessAll

