module Main where


import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


runDay :: [String] -> IO ()
runDay ["1"] = Day1.main
runDay ["2"] = Day2.main
runDay ["3"] = Day3.main
runDay ["4"] = Day4.main
runDay ["5"] = Day5.main
runDay ["6"] = Day6.main
runDay ["7"] = Day7.main
runDay ["8"] = Day8.main
runDay ["9"] = Day9.main
runDay ["10"] = Day10.main
runDay ["11"] = Day11.main
runDay ["12"] = Day12.main
runDay ["13"] = Day13.main
runDay ["14"] = Day14.main
runDay ["15"] = Day15.main
runDay ["16"] = Day16.main
runDay ["17"] = Day17.main
runDay ["18"] = Day18.main
runDay ["19"] = Day19.main
runDay ["20"] = Day20.main
runDay ["21"] = Day21.main
runDay ["22"] = Day22.main
runDay ["23"] = Day23.main
runDay ["24"] = Day24.main
runDay ["25"] = Day25.main
runDay []    = putStrLn "Tell me a day to run."
runDay _     = putStrLn "I've no idea what you're talking about."


main :: IO ()
main = getArgs >>= runDay
