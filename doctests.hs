module Main where
import Test.DocTest

main :: IO ()
main = doctest $ words "--preserve-it src/"
