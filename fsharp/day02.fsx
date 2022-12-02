let (|Rock|Paper|Scissors|) = function
  | "A" | "X" -> Rock 
  | "B" | "Y" -> Paper 
  | "C" | "Z" -> Scissors

let (|Lose|Draw|Win|) = function 
  | "X" -> Lose 
  | "Y" -> Draw 
  | "Z" -> Win

let scoreHand = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let scoreRound (elf: string) (me: string) =
  match elf, me with
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | elf, me when elf = me -> 3
  | _ -> 0

let roundPoints decoder (elf, encodedHand) = 
  let hand = decoder (elf, encodedHand)
  scoreHand hand + scoreRound elf hand

let task1Decoder = function Rock -> "A" | Paper -> "B" | Scissors -> "C"
let task2Decoder = function
  | Rock, Lose -> "C"
  | Rock, Draw -> "A"
  | Rock, Win -> "B"
  | Paper, Lose -> "A"
  | Paper, Draw -> "B"
  | Paper, Win -> "C"
  | Scissors, Lose -> "B"
  | Scissors, Draw -> "C"
  | Scissors, Win -> "A"

let data = 
  System.IO.File.ReadAllLines "data/day02.txt"
  |> Array.map (fun row -> row.Split(" ") |> Array.pairwise |> Array.head)

let totalPoints decoder = data |> Array.sumBy (roundPoints decoder)

printfn "Day 2 Task 1: %i" (totalPoints (snd >> task1Decoder))
printfn "Day 2 Task 2: %i" (totalPoints task2Decoder)
