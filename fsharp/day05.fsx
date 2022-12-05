let data = System.IO.File.ReadAllLines "data/day05.txt"
let stacks = data |> Array.takeWhile (fun row -> row.Contains('['))
let regex = System.Text.RegularExpressions.Regex(@"(?<=\[)(.)(?=\])|( {4})") 

let initial =
  stacks
  |> Array.map (regex.Matches >> Seq.mapi (fun i m -> i, m.Value) >> Seq.toArray)
  |> Array.collect id
  |> Array.groupBy fst
  |> Array.map (
      snd 
      >> Array.map snd 
      >> Array.filter (System.String.IsNullOrWhiteSpace >> not) 
      >> Array.toList)

let shuffle moveOrdering =
  data 
  |> Array.skip (Array.length stacks + 2)
  |> Array.fold (fun acc row ->
    let count :: tail = 
      row.Split([|"move "; " from "; " to "|], System.StringSplitOptions.RemoveEmptyEntries)
      |> Array.map int
      |> Array.toList
    let fromStack :: toStack :: _ = tail |> List.map (fun v -> v - 1)

    acc
    |> Array.updateAt toStack ((List.take count acc.[fromStack] |> moveOrdering) @ acc.[toStack])
    |> Array.updateAt fromStack (List.skip count acc.[fromStack])) initial
  |> Array.choose List.tryHead
  |> String.concat ""

printfn "Day 5 Task 1: %s" (shuffle List.rev)
printfn "Day 5 Task 2: %s" (shuffle id)