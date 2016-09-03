module Main where

import Control.Monad.Free.Church
import Data.Foldable
import Data.List (intercalate)
import UI.View

main :: IO ()
main = putStrLn $ toString $ (list [ text "hello", text "world" ] :: View ())

toString :: Show a => View a -> String
toString = intercalate "\n" . iter go . fmap (pure . show)
  where go (Text s) = [ s ]
        go (List vs) = vs >>= fmap ("- " ++)

toHTML :: Show a => View a -> String
toHTML = intercalate "\n" . iter go . fmap (pure . show)
  where go (Text s) = [ "<p>" ++ s ++ "</p>" ]
        go (List vs) = "<ul>" : (vs >>= fmap (\ s -> "  <li>" ++ s ++ "</li>")) ++ ["</ul>"]
