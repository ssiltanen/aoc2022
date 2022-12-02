let nl = System.Environment.NewLine
let splitBy (delimiter: string) (text: string) = text.Split delimiter

let first :: second :: third :: _ = 
  System.IO.File.ReadAllText "data/day01.txt"
  |> splitBy (nl+nl)
  |> Array.map (splitBy nl >> Array.sumBy int)
  |> Array.sortDescending
  |> Array.toList

printfn "Day 1 Task 1: %i" first
printfn "Day 1 Task 2: %i" (first + second + third)