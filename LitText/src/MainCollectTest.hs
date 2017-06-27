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
--import           Parser.Foundation
-- import           Parser.LinesToParagrahs
import           Uniform.Convenience.StartApp
import           Uniform.FileIO         hiding ((<>))
--import           Uniform.Strings              hiding ((<>), (</>), (<.>))
--import Producer.Servers


programName = "may13 = ProduceLit" :: Text
progTitle = "produce the nt for all markup files (if nt does not exist) " :: Text


main = do
    startProg programName progTitle
        (processAll sourceTest4 generalityTest4 resfile)
        -- defined in processAll

resfile  = makeRelFile "resultCollectTest"




