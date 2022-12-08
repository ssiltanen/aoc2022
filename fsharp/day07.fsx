let (|CdHome|_|) = function [|"$"; "cd"; "/"|] -> Some "/" | _ -> None
let (|CdOut|_|) = function [|"$"; "cd"; ".."|] -> Some ".." | _ -> None
let (|CdIn|_|) = function [|"$"; "cd"; dir|] -> Some dir | _ -> None
let (|File|_|) (cmd: string[]) = 
  match cmd with
  | [|sizeOrDir; _|] -> 
    match System.Int32.TryParse(sizeOrDir) with
    | true, size -> Some size
    | _ -> None
  | _ -> None

let directories = 
  System.IO.File.ReadAllLines "data/day07.txt"
  |> Array.fold (fun (revCwd, files) cmd ->
    let formDir = List.rev >> String.concat "/"
    match cmd.Split(" ") with
    | CdHome _ -> [], files
    | CdOut _ -> List.tail revCwd, files
    | CdIn dir -> dir :: revCwd, (formDir revCwd, 0) :: files
    | File size -> revCwd, (formDir revCwd, size) :: files
    | _ -> revCwd, files ) ([], [])
  |> snd
  |> List.groupBy fst
  |> List.map (fun (dir, group) ->
    (if System.String.IsNullOrEmpty(dir) then "/" else $"/{dir}/"), group |> List.map snd |> List.sum)
  |> List.sortBy fst
  |> List.fold (fun acc (dir, size) ->
    let updatedAcc =
      acc |> List.map (fun (accDir: string, accDirSize) ->
        accDir, if dir.StartsWith(accDir) then accDirSize + size else accDirSize)
    (dir, size) :: updatedAcc
  ) []

directories 
|> List.sumBy (fun (_, size) -> if size <= 100000 then size else 0)
|> printfn "Day 7 Task 1: %i"

let availableSpace = 70000000 - (directories |> List.maxBy snd |> snd)
directories 
|> List.sortBy snd 
|> List.pick (fun (_, size) -> if availableSpace + size >= 30000000 then Some size else None)
|> printfn "Day 7 Task 2: %i"