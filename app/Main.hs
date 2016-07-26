module Main where
import CodeGeneration.CCompiler(compile)
import System.Environment(getArgs)

main :: IO ()
main = do
  (f:_) <- getArgs
  code <- readFile f
  let res = compile f
  writeFile "output.c" res
