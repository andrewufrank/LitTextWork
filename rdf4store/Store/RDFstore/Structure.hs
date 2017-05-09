-----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.Structure
--
-- | conversions to binding values
-- copied from hsparql - Database.HSparql.Connection
-- changed from String to Text
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Store.RDFstore.Structure (structureContentx, BindingValue
                , res2string, res2strings, res2triple
                , node2string, triple2res
                , node3string
                , sparqlResult
                , HTTPresponse
--                , EndPoint
                ) where

import           Uniform.Strings

import           Control.Monad   (guard)
import           Data.Maybe
--import Network.HTTP
--       (Request (..), getResponseBody, simpleHTTP, Request, mkHeader,
--        urlEncodeVars)
import           Data.RDF        as RDF (LValue (..), Node (..), Triple (..),
                                         triple)
import qualified Data.RDF
import           Text.XML.Light
--import qualified Data.RDF.Types as RDF (UNode, LNode)



-- |Local representations of incoming XML results.
data BindingValue = Bound Data.RDF.Node    -- ^RDF Node (UNode, BNode, LNode)
                  | Unbound       -- ^Unbound result value
  deriving (Show, Eq)

-- |Base 'QName' for results with a SPARQL-result URI specified.
sparqlResult :: Text -> QName
sparqlResult s = (unqual . t2s $ s) { qURI = Just "http://www.w3.org/2005/sparql-results#" }

type HTTPresponse = Text  -- is String in hsparql


-- |Transform the 'String' result from the HTTP request into a two-dimensional
--  table storing the bindings for each variable in each row.
-- same as in hsparql, except for HTTPresponse which is Text here not string
structureContentx :: HTTPresponse -> Maybe [[BindingValue]]
structureContentx s =
    do
       e <- doc
       return $ map (projectResult $ vars e) $ findElements (sparqlResult "result") e
    where doc :: Maybe Element
          doc = parseXMLDoc s

          vars :: Element -> [String]
          vars = mapMaybe (findAttr $ unqual "name") . findElements (sparqlResult "variable")

          projectResult :: [String] -> Element -> [BindingValue]
          projectResult vs e = map pVar vs
             where pVar v   = maybe Unbound (value . head . elChildren)
                                        $ filterElement (pred v) e
                   pred v e = isJust $ do a <- findAttr (unqual "name") e
                                          guard $ a == v

          value :: Element -> BindingValue
          value e =
            case qName (elName e) of
              "uri"     -> Bound $ Data.RDF.unode $ (s2t $ strContent e)
              "literal" -> case findAttr (unqual "datatype") e of
                     Just dt -> Bound $ Data.RDF.lnode $
                            Data.RDF.typedL (s2t $ strContent e) (s2t dt)
                     Nothing -> case findAttr langAttr e of
                                  Just lang -> Bound $ Data.RDF.lnode
                                    $ Data.RDF.plainLL (s2t $ strContent e)
                                                (s2t lang)
                                  Nothing   -> Bound $ Data.RDF.lnode $
                                        Data.RDF.plainL (s2t $ strContent e)
              -- TODO: what about blank nodes?
              _         -> Unbound

          langAttr :: QName
          langAttr = blank_name { qName = "lang", qPrefix = Just "xml" }


-- | convert the Bindngvalues to a  reasonable string for a line
res2string :: Bool -> [BindingValue] ->  Text
res2string b ss =  unwords' $ map (node2string b) ss

-- | convert the Bindngvalues to a  reasonable string for a line
res2strings :: Bool -> [BindingValue] ->  [Text]
res2strings b ss =   map (node2string b) ss

node2string ::Bool -> BindingValue -> Text
-- if true, give full string else just the string
--node2string b (Bound (UNode u)) = u
node2string b (Bound (LNode (PlainL s))) = s
node2string b (Bound (LNode (PlainLL s l))) = if b then unwords' [s,l] else s
node2string b (Bound (LNode (TypedL s l))) = if b then unwords' [s,l] else s
--node2string ((Literal s )) = s
node2string b (Bound (UNode s)) =   s
node2string b (Unbound) =  "#unbound#"
node2string b  x =  unwords' ["\nmissing in node2string", s2t . show $ x ,"\n"]

-- | convert to triple

res2triple :: [BindingValue] -> RDF.Triple
res2triple [Bound s,Bound o, Bound p] = RDF.triple s o p
res2triple _ = error "not three values to construct a triple"

triple2res :: RDF.Triple -> [RDF.Node]
triple2res (Triple a b c) =  [a,b,c]
--replacePrefixes

node3string (UNode u) = concat' ["<", u, ">"]
node3string (LNode (PlainL s)) = concat' ["\"", s, "\""]
node3string  x =  unwords' ["\nmissing in node3string", s2t . show $ x ,"\n"]
