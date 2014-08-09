{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Yaml.YDoc

main :: IO()
main = do
  r <- parseYaml =<< readFile "test.yaml"
  let ydoc = YDoc r

  -- print the YDoc 
  putStrLn $ show ydoc

  putStrLn "----"

  -- extract and show YamlLight value
  putStrLn $ show $ yval ydoc



