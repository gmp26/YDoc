{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.YDoc where

import Data.Yaml.YamlLight (parseYaml, YamlLight(..))
import qualified Data.ByteString.Char8 as B
import Data.Map (Map, toList)
import Text.PrettyPrint.HughesPJ (text, (<+>), ($+$), nest, empty, vcat, Doc(..))

newtype YDoc yval = YDoc {yval :: YamlLight}
-- ^YDoc wraps YamlLight values, providing a show that generates serialised yaml text

instance Show (YDoc yval) where 

  show (YDoc yval) = show $ yamlDoc yval where

    yamlDoc :: YamlLight -> Doc
    -- ^Convert a YamlLight value to a pretty print Doc
    yamlDoc yval =
      case yval of
        (YStr bs)   -> text $ B.unpack bs
        (YSeq ymls) -> vcat $ map ((text "-") <+>) (map yamlDoc ymls)
        (YMap m)    -> vcat $ map yamlKVPair (toList m)
        YNil        -> "NULL"

      where 
        yamlKVPair :: (YamlLight, YamlLight) -> Doc
        -- ^ convert a YMap key value pair to a pretty doc
        yamlKVPair (YStr k, yval@(YSeq s)) = vkeyVal k yval
        yamlKVPair (YStr k, yval@(YMap m)) = vkeyVal k yval
        yamlKVPair (YStr k, yval)          = hkeyVal k yval
        yamlKVPair _ = empty

        keyDoc:: B.ByteString -> Doc
        keyDoc k = text ((B.unpack k) ++ ":")

        vkeyVal :: B.ByteString -> YamlLight -> Doc
        -- ^ vertical layout for a key value
        vkeyVal k yval = (keyDoc k) $+$ nest 4 (yamlDoc yval)

        hkeyVal :: B.ByteString -> YamlLight -> Doc
        -- ^ horizontal layout for a key value
        hkeyVal k yval = (keyDoc k) <+> (yamlDoc yval)

