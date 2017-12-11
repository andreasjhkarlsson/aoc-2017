module Day11

open Common
open System

type Direction = N | NW | SW | S | SE | NE
    with
        static member FromString =
            function
            | "n" -> N | "nw" -> NW | "ne" -> NE
            | "s" -> S | "sw" -> SW | "se" -> SE
            | _ -> failwith "invalid input"

        static member Angle direction =
            match direction with // There is 60 degrees between each "direction" in a hexagon
            | NE -> 30.0
            | N -> 90.0
            | NW -> 150.0
            | SW -> 210.0
            | S -> 270.0
            | SE -> 330.0
            |> (*) (Math.PI / 180.0)

        static member All = [N; NW; NE; S; SW; SE] 

        // TODO: Optimize away needlessy having to recompute the cos/sin values every time
        static member Move (x,y) direction = x + Math.Cos(Direction.Angle direction), y + Math.Sin(Direction.Angle direction)

// Euclidean distance
let distance (x1,y1) (x2,y2) = Math.Sqrt((x1-x2)**2.0 + (y1-y2)**2.0) 

// How many steps between the two points, moving only in the hexagon pattern
let rec find home (x,y) =
    if (distance home (x,y)) < 0.5 then 0
    else
        let dir = Direction.All |> List.minBy(Direction.Move (x,y) >> distance home)
        1 + (find home (Direction.Move (x,y) dir))

[<Day(11, "Hex Ed")>]
let solve (input: string) =

    let home = 0.0, 0.0

    // Every position the child process was at
    let positions = 
        input.Trim().Split([|','|])
        |> Array.map Direction.FromString
        |> Array.scan Direction.Move home

    let finalSteps = positions |> Array.last |> find home

    let maxSteps = positions |> Array.map (find home) |> Seq.max

    { Part1 = finalSteps; Part2 = maxSteps }