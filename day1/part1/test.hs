countStuff :: [Int] -> Int
countStuff (x:y:[]) = if y > x then 1 else 0 
countStuff (x:y:xs) = if y > x then countStuff (y:xs) + 1 else countStuff (y:xs)

main = do
  contents <- readFile "list.txt"
  let nums = map (\x -> read x::Int) $ lines contents
      total = countStuff nums
  putStrLn $ show total
