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
run Arguments{..} = putStrLn $ case format of
  String -> toString (list [ text "hello", text "world" ] :: View ())
  HTML -> toHTML (list [ text "hello", text "world" ] :: View ())

toString :: Show a => View a -> String
toString = intercalate "\n" . iter go . fmap (pure . show)
  where go (Text s) = [ s ]
        go (List vs) = vs >>= fmap ("- " ++)

toHTML :: Show a => View a -> String
toHTML = intercalate "\n" . iter go . fmap (pure . show)
  where go (Text s) = [ "<p>" ++ s ++ "</p>" ]
        go (List vs) = "<ul>" : (vs >>= fmap (\ s -> "  <li>" ++ s ++ "</li>")) ++ ["</ul>"]
