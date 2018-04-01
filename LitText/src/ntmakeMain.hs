-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  collect all markup files and process them
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and converts markup to nt
-- takes   cmd line argument: -s gives source dir fuer die markup files.
-- later : -f for the filename
-- later: language (to process the english files first and then the other languages
-- in turn

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Uniform.FileIO         hiding ((<>))

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
import Processor.NTmake


programName = "ntmake6 0.0.4.6" :: Text
progTitle = "produce the lit for all markup files " :: Text

main :: IO ()
main = startProg programName progTitle
            ( parseAndExecuteNTmake
               (unlinesT ["gets the nt files" ])
               "dir relative to home"
            )
