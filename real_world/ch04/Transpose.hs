import System.Environment (getArgs)

transpose :: String -> String
transpose = unlines . transpose' . lines

transpose' :: [[a]] -> [[a]]
transpose' all@(xs : _)
    | null xs = []
    | otherwise = map head all : (transpose' $ map tail all)
transpose' _ = []

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (unlines $ function $ lines input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = transpose'
