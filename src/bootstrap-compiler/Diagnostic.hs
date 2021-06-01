{-# LANGUAGE OverloadedStrings #-}

module Diagnostic where

import Global
import Data.Aeson ( encode, object, KeyValue((.=)), ToJSON(toJSON) )

data Diagnostic =
    Diagnostic
        Severity
        Location
        String

type Severity = Union
    [ Proxy "error"
    , Proxy "warning"
    , Proxy "information"
    , Proxy "hint"
    ]

data Location =
    Location
        !Position
        !Position

data Position =
    Position
        !Int
        !Int

showSeverity = 
    do symbolVal @ "error" @ Proxy
    @>
    do symbolVal @ "warning" @ Proxy
    @>
    do symbolVal @ "information" @ Proxy
    @>
    do symbolVal @ "hint" @ Proxy
    @>
    typesExhausted

instance Show Diagnostic where
    show (Diagnostic severity (Location (Position l c) _) message) = mconcat
        [ "("
        , show l
        , ","
        , show c
        , "): "
        , showSeverity severity
        , ": "
        , message
        ]

instance Data.Aeson.ToJSON Diagnostic where
    toJSON (Diagnostic severity location message) = object
        [ "severity" .= severity
        , "location" .= location
        , "message" .= message 
        ]

instance Data.Aeson.ToJSON Severity where
    toJSON =
        do \(x :: Proxy "error") -> toJSON $ symbolVal x
        @>
        do \(x :: Proxy "warning") -> toJSON $ symbolVal x
        @>
        do \(x :: Proxy "information") -> toJSON $ symbolVal x
        @>
        do \(x :: Proxy "hint") -> toJSON $ symbolVal x
        @>
        typesExhausted

instance Data.Aeson.ToJSON Location where
    toJSON (Location start end) = object
        [ "start" .= start
        , "end" .= end
        ]

instance Data.Aeson.ToJSON Position where
    toJSON (Position lineNumber column) = object
        [ "line-number" .= lineNumber
        , "column" .= column
        ]
