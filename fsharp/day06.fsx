let data = System.IO.File.ReadAllText "data/day06.txt"
let indexOfFirstUniqWindow size =
  Seq.windowed size
  >> Seq.findIndex (Set.ofSeq >> Set.count >> (=) size)
  >> (+) size

printfn "Day 6 Task 1: %i" (indexOfFirstUniqWindow 4 data)
printfn "Day 6 Task 2: %i" (indexOfFirstUniqWindow 14 data)