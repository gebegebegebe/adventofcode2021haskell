countStuff :: [(String, Int)] -> (Int,Int)
countStuff list = foldl func (0,0) list
  where func (accx,accy) ("forward",x) = (accx + x, accy)
        func (accx,accy) ("up",y) = (accx, accy - y)
        func (accx,accy) ("down",y) = (accx, accy + y)

main = do
  contents <- readFile "list.txt"
  let nums = map (\x -> (((words x)!!0), read ((words x)!!1)::Int)) $ lines contents
      totalSet = countStuff nums
      product = fst totalSet * snd totalSet
  putStrLn $ show product 
