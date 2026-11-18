module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, forever)
import Datomic.Connector
import Datomic.EDN
import System.Environment (getArgs, getExecutablePath)
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

check :: Bool -> String -> IO ()
check ok message = unless ok (ioError (userError message))

refused :: IO a -> IO ()
refused action = do
  result <- try (action >> pure ()) :: IO (Either IOException ())
  check (either (const True) (const False) result) "Expected refusal"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--mock", mode] -> mock mode
    _ -> tests

mock :: String -> IO ()
mock mode = case mode of
  "slow" -> do
    _ <- getLine
    threadDelay 500000
    putStrLn "{:ok true :result :abandoned}"
    hFlush stdout
    forever (getLine >> putStrLn "{:ok true :result :next}" >> hFlush stdout)
  "malformed" -> getLine >> putStrLn "garbage" >> hFlush stdout
  "reject" -> do
    _ <- getLine
    putStrLn "{:ok false :error {:message \"rejected\"}}"
    hFlush stdout
    _ <- getLine
    putStrLn "{:ok true :result :recovered}"
    hFlush stdout
  _ -> pure ()

tests :: IO ()
tests = do
  exe <- getExecutablePath
  withConnector exe ["--mock", "slow"] $ \c -> do
    abandoned <- timeout 100000 (query c (Keyword "first") [])
    check (abandoned == Nothing) "First request should be cancelled"
    refused (query c (Keyword "second") [])
  mapM_ (\mode -> withConnector exe ["--mock", mode] $ \c -> do
    refused (query c Nil [])
    refused (query c Nil [])) ["malformed", "exit"]
  withConnector exe ["--mock", "reject"] $ \c -> do
    refused (query c Nil [])
    result <- query c Nil []
    check (result == Keyword "recovered") "A complete rejection must keep the connector usable"
  closed <- withConnector exe ["--mock", "exit"] pure
  refused (query closed Nil [])
  putStrLn "PASS: cancelled, malformed and dead bridges close; rejected requests recover"
