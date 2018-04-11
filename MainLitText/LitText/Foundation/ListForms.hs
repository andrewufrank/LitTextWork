-----------------------------------------------------------------------------
--
-- Module      :  list forms
-- Copyright   :  andrew u frank -
--
-- |  an experiment with a monoid like algebra
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LitText.Foundation.ListForms  where

import Uniform.Strings
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T

--class Zeros a => ListForms l a  where
--    prepend  :: a -> l a -> l a
--    append :: l a -> a -> l a
--    single  :: a -> l a
--    combine :: l a -> l a -> l a
--
--    prepend a la = combine (single a) la
--    append la a = combine la (single a)
--
--class Zeros (LF l) => ListFormsTF l   where
--    type LF l
--    prepend'  :: (LF l) -> l   -> l
--    append' :: l   -> (LF l) -> l
--    single' :: (LF l) -> l
--    combine' :: l  -> l   -> l
--
--    prepend' a la = combine' (single' a) la
--    append' la a = combine' la (single' a)

type URI = Text  -- just for testing

-- a server URI (not including the port, but absolute)
newtype ServerURI = ServerURI {unServerURI :: URI}
                deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkServerURI :: Text -> ServerURI
mkServerURI = ServerURI    -- here check for validity

instance ListForms ServerURI where
    type LF ServerURI = Text
    mkOne t = ServerURI t
    appendTwo a b = ServerURI $ T.append (unServerURI a)  (unServerURI b)

t1 :: ServerURI
t1 = prependOne ("Eins"::Text) (mkOne "Null" )
