YDoc
====

Wraps Data.Yaml.YamlLight providing round-trip yaml -> YamlLight -> yaml

YamLight can parse yaml text into YamlLight values.
It implements show, providing a serialisation of the Haskell YamlLight structure.
This module wraps YamlLight, providing a new show function that
serialises the YamlLight structure to yaml text.

This version builds with ghc-7.8 and cabal-1.18.