-----------------------------------------------------------------------------
--
-- Module      :  Main.BuchToken

--
-- | the different types of text in a markup file
-- TODO would it be ossible to select lines with all caps as hl1?
-- recognize the ending of gutenberg to ignore?
-- uebersetzung und uebersetzer?
    -- for dublin core see <https://de.wikipedia.org/wiki/Dublin_Core>
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module LitText.Foundation.BuchToken_test  where


import           Test.Framework
--import Uniform.TestHarness (testVar3File)
import LitText.Foundation.BuchToken


