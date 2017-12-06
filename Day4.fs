module Day4

open Common


let solvePart1 lines =
    lines
    |> Array.sumBy (fun words ->
        if (Set.ofArray words).Count = words.Length then 1 else 0
    )

let solvePart2 lines = 
    lines
    |> Array.sumBy (fun words ->
        let sorted =
            words
            |> Array.map (fun (word: string) ->
                System.String.Concat(word |> Seq.sort)
            )
        if (Set.ofArray sorted).Count = sorted.Length then 1 else 0
    )
    
[<Day(4)>]
let solve (input: string) =

    let words = parseMatrix id input

    { Part1 = solvePart1 words; Part2 = solvePart2 words }