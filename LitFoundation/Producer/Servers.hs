-----------------------------------------------------------------------------
--
-- Module      :  Foundation - servers
-- Copyright   :  andrew u frank -
--
-- | the addresses of the servers
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Producer.Servers (
    module Producer.Servers
    ) where


import           Data.RDF.Extension
import Uniform.Strings

--type PartURI = Text   -- should be defined in uniform.http?? todo
        -- is defined in RDF.Extension

serverLocalhost, serverBrest :: PartURI

serverLocalhost = localhost
serverBrest = "nlp.gerastree.at"

localhost = "127.0.0.1"

