{-# OPTIONS_GHC -F -pgmF hapi-idlc -optF ./idl/github.idl -optF plugin #-}

module Main where

import Web.Hapi.Plugin.Interface

main :: IO ()
