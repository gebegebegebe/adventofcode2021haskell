countStuff :: [Int] -> Int
countStuff (x:[]) = 0
countStuff (x:xs) = if (sum $ take 3 (x:xs)) < (sum $ take 3 (xs)) then countStuff xs + 1 else countStuff xs

main = do
  contents <- readFile "list.txt"
  let nums = map (\x -> read x::Int) $ lines contents
  putStrLn $ show $ countStuff nums 
