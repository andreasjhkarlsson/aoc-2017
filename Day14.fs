module Day14

open Common

type Square = Free | Used 

// Cartesian product operator
let (|*|) l1 l2 =
    l1
    |> List.map (fun e1 -> l2 |> List.map (fun e2 -> e1, e2))
    |> List.concat

[<Day(14, "Disk Defragmentation")>]
let solve (input: string) =
    
    let input = input.Trim()

    let grid =
        Array.init 128 (fun row ->
            let hash = Day10.knotHash <| sprintf "%s-%d" input row
            Array.init 128 (fun col ->
                if (hash.[col / 8] &&& (1uy <<< (7 - (col % 8)))) > 0uy
                then Used else Free
            )
        )

    let usedCount = grid |> Array.concat |> Array.sumBy (fun s -> if s = Used then 1 else 0)

    let rec adjacents visited (x,y)  =

        if visited |> Set.contains (x,y) then visited
        else if x < 0 || x > 127 || y < 0 || y > 127 then visited
        else if grid.[x].[y] = Free then visited
        else
            [-1,0; 1,0; 0,-1; 0,1]
            |> List.map (fun (dx, dy) -> x + dx, y + dy)
            |> List.fold adjacents (Set.add (x,y) visited)
    
    let regions =
        [0..127] |*| [0..127]
        |> List.fold (fun (visited, count) (x,y) ->
            if visited |> Set.contains (x,y) || grid.[x].[y] = Free then
                visited, count
            else
                adjacents visited (x,y), count + 1 
        ) (Set.empty, 0)
        |> snd

    { Part1 = usedCount; Part2 = regions }