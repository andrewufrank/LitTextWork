-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  test NLP services
-- Copyright   :  andrew u frank -
--


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main     where      -- must have Main (main) or Main where

-- all must have {-@ HTF_TESTS @-}

import  Uniform.Strings
import  Test.Framework

import {-@ HTF_TESTS @-} Parser.ProduceDocCallNLP


main =  do
    putStrLn "TestNLP.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    putStrLn "TestNLP.hs end ------------- \n"
    return ()



