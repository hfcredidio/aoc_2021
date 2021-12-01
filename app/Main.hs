module Main where


import System.Environment
import qualified Day1


runDay :: [String] -> IO ()
runDay ["1"] = Day1.main
runDay []    = putStrLn "Tell me a day to run."
runDay _     = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
