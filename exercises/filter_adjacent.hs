{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <=<" #-}
import Data.List
import Control.Monad

-- filterAdjacent: Filter einer Liste, wobei Element nur erhalten bleibt, wenn ident mit nachfolgendem, "abbbccd" -> "bbc"
filterAdjacentMonad :: Eq a => [a] -> [a]
filterAdjacentMonad = (init =<<) . group

filterAdjacent :: Eq a => [a] -> [a]
filterAdjacent = concatMap init . group