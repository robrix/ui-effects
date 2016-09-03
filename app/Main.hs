module Main where

import Control.Monad.Free.Church
import Data.Foldable
import UI.View

main :: IO ()
main = traverse_ putStrLn $ toString $ list [ text "hello", text "world" ]

toString :: View [String] -> [String]
toString = iter $ \case
  Text s -> [ s ]
  List vs -> vs >>= fmap ("- " ++)
