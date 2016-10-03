module Main where

import Control.Monad.Free.Freer
import Data.Foldable
import Options.Applicative
import System.IO
import UI.View

data Arguments = Arguments { format :: Format }
data Format = CLI

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*>
          (Arguments
          <$> flag CLI CLI (long "cli" <> short 'C')))
          (fullDesc <> progDesc "UI playground")

run :: Arguments -> IO ()
run Arguments{..} = case format of
  CLI -> cli view
  where view = list [ text "hello", input >>= text ] >> pure ()

cli :: View () -> IO ()
cli = iterM $ \ view -> case view of
  Text s -> putStrLn s
  List vs -> traverse_ (putStr "- " >>) vs
  Input f -> putStr "> " >> hFlush stdout >> getLine >>= f
