module Day24

open Common

module Bridge =

    [<ReferenceEquality>]
    type Component =
        { In: int; Out: int }
        with
            static member fits input { In = ``in``; Out = out } = input = ``in`` || input = out
            static member flip { In = ``in``; Out = out } = { In = out; Out = ``in`` }

    let strength = List.sumBy (fun {In = ``in``; Out = out} -> ``in`` + out)

    let length = List.length

    let rec build input components construction target =
        match components |> List.filter (Component.fits input) with
        | [] -> construction
        | possibleInputs ->
            possibleInputs |> List.map (fun comp ->
                let components = components |> List.except [comp]
                let comp = if comp.In = input then comp else Component.flip comp
                let construction = comp :: construction

                build comp.Out components construction target
            )
            |> List.maxBy target

[<Day(24, "Electromagnetic Moat")>]
let solve input = 
    
    let components =
        parseLines input
        |> Array.map (split "/" >> fun a -> {Bridge.Component.In = int a.[0]; Bridge.Component.Out = int a.[1]})
        |> Array.toList
    
    // Build bridge with strength as target
    let part1 = Bridge.strength <| Bridge.build 0 components [] Bridge.strength

    let intToFraction n = (float n) / 10.0 ** (ceil(log10(float n))) // 1234 -> 0.1234

    // Build bridge with length as target with strength as secondary
    let part2 = Bridge.strength <| Bridge.build 0 components [] (fun b -> (float <| Bridge.length b) + (intToFraction (Bridge.strength b)))

    { Part1 = part1; Part2 = part2 }