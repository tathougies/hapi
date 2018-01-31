module Main where

import Web.Hapi.Plugin.Interface

main :: IO ()
main = pluginMain Plugin { _pluginName = "com.github" }
