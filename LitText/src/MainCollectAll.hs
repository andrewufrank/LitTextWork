-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  collect all markup files and process them
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and puts the text into the db
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Processor.ProcessAll
import           Parser.Foundation
-- import           Parser.LinesToParagrahs
import           Uniform.Convenience.StartApp
import           Uniform.FileIO         hiding ((<>))
import           Uniform.Strings              hiding ((<>), (</>), (<.>))
-- for the command line parser:
--import           Options.Applicative
-- the http://hackage.haskell.org/package/optparse-applicative package
--import           Data.Semigroup               ((<>))
--import           Options.Applicative.Builder

programName = "may13 = ProduceLit" :: Text
progTitle = "produce the lit for all markup files " :: Text


main = do
    startProg programName progTitle
        (processAll textstateX resfile)

origDirForTest = "/home/frank/additionalSpace/DataBig/LitTest" :: FilePath
resfile  = makeRelFile "resultCollectAll"

textstateX = TextState2 {
        endpoint = "http://127.0.0.1:3030/testDB/update"
        , serverLoc = "http://127.0.0.1"
        , originalsDir = makeAbsDir origDirForTest
        , authorDir = ""
        , buchname = ""
        , textfilename = makeAbsFile ("/home/frank/additionalSpace/DataBig/LitOriginals/notafile")
 --       , graph = "Test1"
        }

--main1 = processAll textstateX resfile
