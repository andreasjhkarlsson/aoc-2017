module Day1
   
open System
open Common

let sumAdjecents offset (input: string) =
    input
    |> Seq.mapi (fun i d -> d, input.[(i+offset) % input.Length]) // Map each digit to its following neighbour
    |> Seq.filter (fun (d,n) -> d = n) // Only keep matching pairs
    |> Seq.sumBy (fst >> Char.GetNumericValue >> int) // Sum remaining digits        

let solvePart1 = sumAdjecents 1

let solvePart2 input = sumAdjecents ((String.length input) / 2) input

[<Day(1)>]
let solve input = { Part1 = solvePart1 input; Part2 = solvePart2 input} 
