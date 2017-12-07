
open System
open Common

[<EntryPoint>]
let main argv = 
    
    printfn "*** Advent of Code 2017 in F# ***"

    let day =
        if argv.Length > 0 then int argv.[0]
        else printfn "Day to solve: "; int <| Console.ReadLine ()

    let input =
        if argv.Length > 1 then argv.[1]
        else printfn "Input to puzzle: "; Console.ReadAll ()

    let days = DayAttribute.All

    match days |> List.tryFind (fun (d,_) -> d.Day = day) with
    | Some (day, fn) ->
        let solution = fn.Invoke(null, [|input|])
        printfn "*** Solution for day %d ***" day.Day
        printfn "Part 1: %A" <| solution.GetType().GetProperty("Part1").GetValue(solution)
        printfn "Part 2: %A" <| solution.GetType().GetProperty("Part2").GetValue(solution)
    | None -> printfn "This day has not been solved"

    0
