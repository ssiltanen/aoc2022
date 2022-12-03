let charToPriority c =
  if System.Char.IsLower(c) then int c - 96
  else int c - 38

let data = System.IO.File.ReadAllLines "data/day03.txt"

data
|> Array.sumBy (
    seq 
    >> Seq.splitInto 2 
    >> Seq.map set 
    >> Set.intersectMany
    >> Seq.head
    >> charToPriority)
|> printfn "Day 3 Task 1: %i"

data
|> Array.chunkBySize 3
|> Array.sumBy (
  Array.map set
  >> Set.intersectMany
  >> Seq.head
  >> charToPriority)
|> printfn "Day 3 Task 2: %i"
