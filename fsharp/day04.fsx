let assignmentPairs = 
  System.IO.File.ReadAllLines "data/day04.txt"
  |> Array.map (fun row -> row.Split('-', ',') |> Array.map int)

let completeOverlap [| aStart; aEnd; bStart; bEnd |] = 
  (aStart <= bStart && aEnd >= bEnd) || (aStart >= bStart && aEnd <= bEnd)

let partialOverlap [| aStart; aEnd; bStart; bEnd |] = 
  (aEnd >= bStart && aStart <= bEnd) || (bEnd >= aStart && bStart <= aEnd)

let (task1, task2) = 
  assignmentPairs
  |> Array.fold (fun (task1, task2) pairs -> 
    task1 + System.Convert.ToInt32(completeOverlap(pairs)),
    task2 + System.Convert.ToInt32(partialOverlap(pairs))) 
    (0,0)

printfn "Day 4 Task 1: %i" task1
printfn "Day 4 Task 2: %i" task2
