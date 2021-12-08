module Main where


import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7


runDay :: [String] -> IO ()
runDay ["1"] = Day1.main
runDay ["2"] = Day2.main
runDay ["3"] = Day3.main
runDay ["4"] = Day4.main
runDay ["5"] = Day5.main
runDay ["6"] = Day6.main
runDay ["7"] = Day7.main
runDay []    = putStrLn "Tell me a day to run."
runDay _     = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
