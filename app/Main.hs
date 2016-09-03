module Main where

import Control.Monad.Free.Church
import Data.List (intercalate)
import Options.Applicative
import UI.View

data Arguments = Arguments { format :: Format }
data Format = String | HTML

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*>
          (fmap Arguments
          $   flag String String (long "string" <> short 'S')
          <|> flag' HTML (long "html" <> short 'H')))
          (fullDesc <> progDesc "UI playground")

run :: Arguments -> IO ()
run Arguments{..} = case format of
  String -> cli view
  HTML -> putStrLn $ toHTML view
  where view = list [ text "hello", input >>= text ] :: View ()

cli :: View () -> IO ()
cli = iterM go
  where go (Text s) = putStrLn s
        go (List vs) = sequence_ vs
        go (Input f) = getLine >>= f

toHTML :: Show a => View a -> String
toHTML = intercalate "\n" . iter go . fmap (pure . show)
  where go (Text s) = [ "<p>" ++ s ++ "</p>" ]
        go (List vs) = "<ul>" : (vs >>= fmap (\ s -> "  <li>" ++ s ++ "</li>")) ++ ["</ul>"]
        go (Input f) = [ "<input type='text' />" ]
