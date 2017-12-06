module Day2

open Common

let solvePart1 matrix =
    matrix
    |> Array.sumBy (fun line ->
        (Array.max line) - (Array.min line)
    )

let solvePart2 matrix =
    matrix
    |> Array.sumBy (fun line ->
        line
        |> Seq.ofArray
        |> Seq.choose (fun n ->
            line
            |> Array.tryFind (fun d -> d <> n &&  (n%d) = 0) // Find divedend of n (that is not n itself)
            |> Option.map (fun divedend -> n / divedend) // Map divedend to actual result
        )
        |> Seq.head // According to puzzle there should only be one matching division pair
    )
    
[<Day(2)>]
let solve input =
    let matrix = parseMatrix int input 

    { Part1 = solvePart1 matrix; Part2 = solvePart2 matrix}