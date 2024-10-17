module Main where

import Type_Definitions
import Evaluators


main :: IO ()
main = do
    let initialEnv = initializeEnv exampleInstructions
    (_, finalEnv) <- runInterp (runInterpreter) initialEnv
    putStrLn $ "\nfinal state: \n" 
    putStrLn $  "Registers: " ++ show (registers finalEnv) 
    putStrLn $  "Memmory: " ++ show (memoryEnv finalEnv)  

