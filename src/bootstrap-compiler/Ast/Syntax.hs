{-# LANGUAGE OverloadedStrings #-}

module Ast.Syntax where

import Data.Aeson
import qualified Ast
import qualified Diagnostic

type instance Ast.Attributes "syntax-analyzed" a = Attributes

data Attributes =
    Attributes
        { location :: !Diagnostic.Location
        }

instance Data.Aeson.ToJSON Attributes where
    toJSON (Attributes location) = object
        [ "location" .= location
        ]
