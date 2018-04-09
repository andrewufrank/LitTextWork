-----------------------------------------------------------------------------
--
-- Module      :  Data.RDF.FileTypes
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  af
-- Stability   :
-- Portability :
--
-- | the definition of filetypes for turtle (ttl) and ntriples (nt)
-- but when the triples as text, then read file with readFile2
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeFamilies  #-}
--{-# Option -w #-}

module Data.RDFext.FileTypes_test  where
import           Test.Framework
import Data.RDFext.FileTypes


testdir = makeAbsFile  "/home/frank/test/empty"
test_writehandle = do
    r <- runErr $ do
            bracketErrIO
                (openHandle6 testdir ntFileTriples)
                (closeHandle6 testdir  ntFileTriples)
                (\h -> writeHandle6 h ntFileTriples ([]::[Triple]))
    putIOwords ["test_writehandle result", showT r]
    assertEqual True True
