module Main where


import System.Environment
import qualified Day1
import qualified Day2


runDay :: [String] -> IO ()
runDay ["1"] = Day1.main
runDay ["2"] = Day2.main
runDay []    = putStrLn "Tell me a day to run."
runDay _     = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
