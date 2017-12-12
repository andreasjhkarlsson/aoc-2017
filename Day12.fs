module Day12

open Common

[<Day(12, "Digital Plumber")>]
let solve (input: string) =
    
    let nodes =
        parseLines input
        |> Array.map (fun line ->
            match line with
            | Regex @"(\d+) <-> (.*)" [id; connectionList] ->
                int id, connectionList.Split([|','|]) |> Array.map (fun s -> s.Trim() |> int) |> Array.toList
            | _ -> failwith "invalid input" 
        )
        |> Map.ofArray
    
    let rec mark nodes marks node =
        if marks |> Set.contains node then marks
        else
            let marks = marks |> Set.add node

            nodes
            |> Map.find node
            |> List.map (mark nodes marks)
            |> Set.unionMany

    let rec groups nodes =
        if nodes = Map.empty then 0
        else
            let group =
                nodes
                |> (Map.toSeq >> Seq.head >> fst)
                |> mark nodes Set.empty

            nodes
            |> Map.filter (fun id _ -> group |> Set.contains id |> not)
            |> groups
            |> (+) 1


    let group0 = mark nodes Set.empty 0
    let part1 =
        nodes
        |> Map.filter (fun id _ -> group0 |> Set.contains id)
        |> (Map.toList >> List.length)

    {Part1 = part1; Part2 = groups nodes}
