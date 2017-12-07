module Day6

open Common

let clone (array: int[]) = array.Clone () :?> int array

type Result = { Total: int; Loop: int}

let rec cycle banks previous =
    // Find target bank
    let index, free = banks |> Array.mapi (fun i v -> i,v) |> Array.maxBy snd
    banks.[index] <- 0

    // Redistribute
    for i = 1 to free do
        let nextIndex = (index + i) % banks.Length
        banks.[nextIndex] <- banks.[nextIndex] + 1

    // Did we find a loop?
    if previous |> Map.containsKey banks then
        {Total = previous.Count + 1; Loop = previous.Count - previous.[banks]}
    else
        previous
        |> Map.add (clone banks) previous.Count // Store current bank configuration + current iteration count in map
        |> cycle banks

[<Day(6, "Memory Reallocation")>]
let solve input = 
    
    let banks = parseList int input

    let result = (cycle banks Map.empty) 
    
    { Part1 = result.Total; Part2 = result.Loop }
