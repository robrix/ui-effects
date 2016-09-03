module Main where

import Control.Monad.Free.Church
import Options.Applicative
import UI.View

data Arguments = Arguments { format :: Format }
data Format = String

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*>
          (fmap Arguments
          $   flag String String (long "string" <> short 'S')))
          (fullDesc <> progDesc "UI playground")

run :: Arguments -> IO ()
run Arguments{..} = case format of
  String -> cli view
  where view = list [ text "hello", input >>= text ] :: View ()

cli :: View () -> IO ()
cli = iterM go
  where go (Text s) = putStrLn s
        go (List vs) = sequence_ vs
        go (Input f) = getLine >>= f
