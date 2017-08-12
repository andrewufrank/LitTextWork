 -- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  Main production
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and puts the text into the db
-- corresponds to main collect for single files
-- for now keep the two distinct
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
import           Options.Applicative
import Processor.ProcessAll -- for the destination and source

programName = "main production" :: Text
progTitle = "put file into the store  " :: Text


main = startProg programName progTitle
        (parseAndExecute
            (unlinesT ["converts a text.markup file to .nt ntriple files"
            , "xx"])
        "test/origin author buch"
        )

--- cmd line parsing
data LitArgs = LitArgs
  { argOrigTest   :: String   -- ^ orig or test to decide where to take the file
  , argdir   :: String   -- ^ the subdirectory in originals
                        -- where the markup file is
                        -- the same dirname is used in convertsDir
  , argbuch  :: String -- ^ the filename in the dir
--  , arggraph :: String   -- ^ is attached to gerstreeURI for graph name
  }

cmdArgs :: Parser (LitArgs)
cmdArgs = LitArgs
     <$> argument str
          (
--          long "test" <>
          metavar "(Orig or Test)"
         <> help "orig or test" )
     <*> argument str
          (
          metavar "Author"
         <> help "subdirectory name in LitOriginal - author" )
     <*> argument str
          (
          metavar "buch"
         <> help "buch - filename " )


parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2   = do
        args <- callIO $ execParser opts
--        let textstate = cmd2textstate args
        let generality  = if isPrefixOf' "o" (argOrigTest args)
                then generalityOrig4
                else generalityTest4
        let source = if isPrefixOf' "o" (argOrigTest args)
                then sourceOrig4
                else sourceTest4
        let textstate = fillTextState2 source generality
                 (argdir args) (argbuch args)
        mainLitAndNLPproduction False  textstate
      where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))


cmd2textstate :: LitArgs -> TextState2
-- fillst the textstate with directory and filename and proj
-- language is by default english, buchcode is the same as proj
cmd2textstate args  = fillTextState2 sourceTest4 generalityTest4
                 (argdir args) (argbuch args)

