 -- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  Main production
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and puts the text into the db
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Main2sub                     (mainLitAndNLPproduction)
import           Parser.Foundation
-- import           Parser.LinesToParagrahs
import           Uniform.Convenience.StartApp
import           Uniform.FileIO
import           Uniform.Strings              hiding ((<>))
-- for the command line parser:
import           Options.Applicative
-- the http://hackage.haskell.org/package/optparse-applicative package
import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder

programName = "main production" :: Text
progTitle = "put file into the store  " :: Text


main = startProg programName progTitle
        (parseAndExecute
            (unlinesT ["converts a text.markup file in several .nt ntriple files"
            , "litmain subdirname filename project buch language"])
        "litmain subdirname filename project buch language"
        (mainLitAndNLPproduction False) -- implied TextState as argument, returned by parseAndExecute
        )

--- cmd line parsing
data LitArgs = LitArgs
  { argdir   :: String   -- ^ the subdirectory in originals
                        -- where the markup file is
                        -- the same dirname is used in convertsDir
  , argbuch  :: String -- ^ the filename in the dir
  , arggraph :: String   -- ^ is attached to gerstreeURI for graph name
  }

cmdArgs :: Parser LitArgs
cmdArgs = LitArgs
     <$> argument str
          (
        --   long "subdir" <>
          metavar "STRING"
         <> help "subdirectory name in LitOriginal - author" )
     <*> argument str
          (
        --   long "filename" <>
          metavar "STRING"
         <> help "buch - filename " )
     <*> argument str
          (
        --   long "filename" <>
          metavar "STRING"
         <> help "graph " )


parseAndExecute  :: Text -> Text -> (TextState2  -> ErrIO () ) -> ErrIO ()
parseAndExecute t1 t2 op  = do
        args <- callIO $ execParser opts
        let textstate = cmd2textstate args
        op textstate
      where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))


cmd2textstate :: LitArgs -> TextState2
-- fillst the textstate with directory and filename and proj
-- language is by default english, buchcode is the same as proj
cmd2textstate args  = TextState2
        {
          endpoint = "http://nlp.gerastree.at:3030/aprilDB/update"
          , serverLoc = "http://nlp.gerastree.at"
          -- endpoint = "http://127.0.0.1:3030/marchDB/update"
        -- , originalsDir = mkFilepath lpX "/home/frank/testLit/"
        , originalsDir = mkFilepath lpX "/home/frank/additionalSpace/DataBig/LitOriginals/"
        -- , originalsDir = mkFilepath lpX "/home/frank/additionalSpace/DataBig/LitTest/"
                    -- waterhouse/kuw.markup
        , authorDir = s2t . argdir $ args
        , buchname = s2t . argbuch $ args
        , graph = s2t . arggraph $ args
        }
