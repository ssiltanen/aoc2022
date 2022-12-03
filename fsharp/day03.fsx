let charToPriority c =
  if System.Char.IsLower(c) then int c - 96
  else int c - 38

let data = System.IO.File.ReadAllLines "data/day03.txt"

data
|> Array.sumBy (
    seq 
    >> Seq.splitInto 2 
    >> Seq.map set 
    >> Seq.reduce Set.intersect 
    >> Set.toList 
    >> List.head
    >> charToPriority)
|> printfn "Day 2 Task 1: %i"

data
|> Array.chunkBySize 3
|> Array.sumBy (
  Array.map (seq >> set)
  >> Array.reduce Set.intersect
  >> Set.toList
  >> List.head
  >> charToPriority)
|> printfn "Day 2 Task 2: %i"



