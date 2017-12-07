module Day3

open System
open Common

type Direction = Left | Up | Right | Down

let rec spiral target i n direction (lx, ly) =
    if n > target then []
    elif i = 0 then ((0,0),1) :: (spiral target 1 2 Right (0,0))
    else
        let newPoints =
            [0..((i-1) / 2)]
            |> List.map(fun s ->
                match direction with
                | Left -> (lx-(s+1), ly), n + s
                | Right -> (lx+(s+1), ly), n + s
                | Up -> (lx, ly+(s+1)), n + s
                | Down -> (lx, ly - (s+1)), n + s
            )
        let nextDirection = match direction with | Left -> Down | Right -> Up | Up -> Left | Down -> Right
        let np, nn = newPoints |> List.rev |> List.head
        newPoints @ (spiral target (i+1) (nn+1) nextDirection np)

let spiral2 target =
    let result: (int option) ref = ref None

    let rec _spiral2 i n direction (lx, ly) results =
    
        if n > target then
            (!result).Value
        elif i = 0 then (_spiral2 1 2 Right (0,0) (results |> Map.add (0,0) 1))
        else
            
            let newMap = ref results

            let adjecent (x,y) =
                let sum = ref 0
                for ox in [-1..1] do
                    for oy in [-1..1] do
                        match !newMap |> Map.tryFind (x+ox,y+oy) with
                        | Some n ->
                            sum := !sum + n
                            ()
                        | None -> ()
                !sum
            let newPoints =
                [0..((i-1) / 2)]
                |> List.map(fun s ->
                    let np =
                        match direction with
                        | Left -> (lx-(s+1), ly)
                        | Right -> (lx+(s+1), ly)
                        | Up -> (lx, ly+(s+1))
                        | Down -> (lx, ly - (s+1))
                    let sum = (adjecent np)
                    if sum > target && (!result).IsNone then
                        result := Some sum
                    newMap := !newMap |> Map.add np sum
                    np, sum
                )
            let nextDirection = match direction with | Left -> Down | Right -> Up | Up -> Left | Down -> Right
            let np, nn = newPoints |> List.rev |> List.head
            (_spiral2 (i+1) (nn+1) nextDirection np !newMap)
    _spiral2 0 0 Right (0,0) Map.empty
        
let solvePart1 target =
    let (x,y), _ = spiral target 0 1 Right (0,0) |> List.find (fun (p, n) -> n = target)
    
    Math.Abs(x) + Math.Abs(y)

let solvePart2 = spiral2

[<Day(3)>]
let solve (input: string) =
    let target = input.Trim() |> int

    { Part1 = solvePart1 target; Part2 = solvePart2 target }