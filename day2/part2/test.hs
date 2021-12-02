countStuff :: [(String, Int)] -> (Int,Int,Int)
countStuff list = foldl func (0,0,0) list
  where func (aim, accx, accy) ("forward",x) = (aim, accx + x, accy + aim*x)
        func (aim, accx, accy) ("up",y) = (aim - y, accx, accy)
        func (aim, accx, accy) ("down",y) = (aim + y, accx, accy)

main = do
  contents <- readFile "list.txt"
  let nums = map (\x -> (((words x)!!0), read ((words x)!!1)::Int)) $ lines contents
      totalSet = countStuff nums
  putStrLn $ show $ (\(_,x,y) -> x*y) totalSet
