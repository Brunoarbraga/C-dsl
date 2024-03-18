module FileGen where

import DSL
import System.IO

writeTemplate :: Code a -> String
writeTemplate generatedCode = unlines $
  [ "#include <stdio.h>", "", "int main() {"] ++ lines (runCode generatedCode) ++ [ "  return 0;", "}"]

writeCFile :: FilePath -> Prog a -> IO ()
writeCFile filepath prog = do
    let cCode = writeTemplate (code prog)
    System.IO.writeFile filepath cCode