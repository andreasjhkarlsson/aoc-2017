module Day19

open Common

type Direction =
    Up | Down | Left | Right
    with
        static member Move (x,y) =
            function Up -> (x,y-1) | Down -> (x,y+1) | Left -> (x-1,y) | Right -> (x+1,y)

type Symbol = Vertical | Horizontal | Corner | Letter of char | Empty

type Map = Symbol [][]

[<Day(19,"A Series of Tubes")>]
let solve (input: string) =

    let map =
        input.Split([|'\n'|])
        |> Array.choose (fun line -> match line.Trim([|'\r'|]) with | "" -> None | line -> Some line)
        |> Array.map (fun line ->
            line
            |> Seq.map (
                function
                | '|' -> Vertical   | '-' -> Horizontal
                | '+' -> Corner     | ' ' -> Empty      | c -> Letter c
            )
            |> Seq.toArray
        )

    let start, direction =
        let (|?|) a b = if Option.isSome a then a else b

        (Seq.init map.[0].Length id |> Seq.tryPick (fun x -> if map.[0].[x] = Vertical then Some((x,0),Down) else None))
        |?|
        (Seq.init map.[map.Length-1].Length id |> Seq.tryPick (fun x -> if map.[map.Length-1].[x] = Vertical then Some ((x, map.[map.Length-1].Length),Up) else None))
        |?|
        (Seq.init map.Length id |> Seq.tryPick (fun y -> if map.[y].[0] = Horizontal then Some ((0,y),Right) else None))
        |?|
        (Seq.init map.Length id |> Seq.tryPick (fun y -> if map.[y].[map.[y].Length-1] = Horizontal then Some ((map.[y].Length-1,y),Left) else None))
        |> Option.get

    let rec walk (x, y) direction walked =
        let height = map.Length
        let width = map.[y].Length
        let current = map.[y].[x]
        let walked = current :: walked

        let tryMove direction =
            let x,y = direction |> Direction.Move (x,y)
            if x >= 0 && x < width && y >= 0 && y < height && map.[y].[x] <> Empty then 
                Some (x,y)
            else None

        let canMove = tryMove >> Option.isSome

        match map.[y].[x] with
        | Empty -> walked |> List.rev
        | current ->
            let newDirection =
                match current with
                | Corner ->
                    match direction with
                    | Down  | Up    when canMove Left   -> Left
                    | Down  | Up    when canMove Right  -> Right
                    | Right | Left  when canMove Up     -> Up
                    | Right | Left  when canMove Down   -> Down
                    | _ -> failwith "I'm lost :("
                | _ -> direction

            match tryMove newDirection with
            | None -> walked |> List.rev
            | Some (x,y) -> (walk (x, y) newDirection walked)
                
    let walked = walk start direction [] 

    let part1 = walked  |> List.fold (fun str -> function Letter l -> str + (string l) | _ -> str) ""

    let part2 = walked |> List.length

    { Part1 = part1; Part2 = part2 }