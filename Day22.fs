module Day22


open Common

type Turn = Left | Right | Straight | Reverse

type Direction =
    North | West | East | South
    with
        static member Move direction (x,y) =
            let (dx,dy) = match direction with North -> (0,-1) | West -> (-1,0) | East -> (1,0) | South -> (0,1)
            (x+dx,y+dy)
        static member Turn facing turn =
            match facing with
            | West when turn = Turn.Left -> South
            | West when turn = Turn.Right -> North
            | West when turn = Reverse -> East
            | East when turn = Turn.Right -> South
            | East when turn = Turn.Left -> North
            | East when turn = Reverse -> West
            | North when turn = Turn.Left -> West
            | North when turn = Turn.Right -> East
            | North when turn = Reverse -> South
            | South when turn = Turn.Left -> East
            | South when turn = Turn.Right -> West
            | South when turn = Reverse -> North
            | _ when turn = Turn.Straight -> facing
            | _ -> failwith "Invalid direction"

type Status =
    | Clean
    | Weakened
    | Infected
    | Flagged

type Mode = Basic | Evolved

let rec virus mode (x,y) facing infections = seq {

        let turnTo, newStatus =

            match infections |> Map.tryFind (x,y) with
            | Some Infected when mode = Basic -> 
                Right, Clean
            | Some Clean when mode = Basic ->
                Left, Infected
            | None when mode = Basic ->
                Left, Infected
            | None when mode = Evolved ->
                Left, Weakened
            | Some Clean when mode = Evolved ->
                Left, Weakened
            | Some Weakened when mode = Evolved ->
                Straight, Infected
            | Some Infected when mode = Evolved -> 
                Right, Flagged
            | Some Flagged when mode = Evolved ->
                Reverse, Clean
            | _ -> failwith "Virus crashed"
        
        yield (x,y), newStatus

        let infections = infections |> Map.add (x,y) newStatus
        let facing = Direction.Turn facing turnTo
        let (x,y) = Direction.Move facing (x,y)
        yield! virus mode (x,y) facing infections     
    }

[<Day(22, "Sporifica Virus")>]
let solve input =

    let matrix = parseLines input |> Array.map (Seq.map string >> Seq.toArray)
    
    let startingInfections =
        matrix
        |> Array.mapi (fun y row ->
            row
            |> Array.mapi (fun x c -> if c = "#" then ((x,y), Infected) else (x,y), Clean)
        )
        |> Array.concat
        |> Map.ofArray

    let countInfections mode iters =

        virus mode (matrix.Length / 2, matrix.[0].Length / 2) North startingInfections
        |> Seq.take iters
        |> Seq.fold (fun count (_, status) ->
            if status = Infected then
                count + 1
            else count
        ) 0        
        
    let part1 = countInfections Basic 10000
    let part2 = countInfections Evolved 10000000
        
    { Part1 = part1; Part2 = part2 }



