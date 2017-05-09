-----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.Test
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module justTEst Store.RDFstore.Test where

import qualified Data.Text.IO as T
import Uniform.Strings

y = "satwett" :: Text

main = do putStrLn   "Hello as String!"
          T.putStrLn "Hello as Text!"

