let (task1, task2) = 
  System.IO.File.ReadAllLines "data/day04.txt"
  |> Array.fold (fun (task1, task2) row -> 
    let [| a1; a2; b1; b2 |] = row.Split('-', ',') |> Array.map int
    task1 + System.Convert.ToInt32((a1 <= b1 && a2 >= b2) || (a1 >= b1 && a2 <= b2)),
    task2 + System.Convert.ToInt32((a2 >= b1 && a1 <= b2) || (b2 >= a1 && b1 <= a2))) 
    (0,0)

printfn "Day 4 Task 1: %i" task1
printfn "Day 4 Task 2: %i" task2