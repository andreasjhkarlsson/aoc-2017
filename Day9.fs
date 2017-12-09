module Day9

open Common

type Mode = Group | Garbage

let rec parse mode score stream =

    let inside = (=) mode

    match stream with
    | head::rest ->
        match head with
        | '{' when inside Group -> let subscore, gc = (parse Group (score + 1) rest) in score + subscore, gc
        | '}' when inside Group -> parse Group (score - 1) rest
        | '<' when inside Group -> parse Garbage score rest
        | '>' when inside Garbage -> parse Group score rest
        | '!' when rest <> [] -> parse mode score (List.tail rest)
        | _ when inside Garbage -> let subscore, gc = parse mode score rest in subscore, gc + 1
        | _ -> parse mode score rest
    | [] -> 0, 0
    

[<Day(9,"Stream Processing")>]
let solve (input: string) =
    Solution<int,int>.Pair <| parse Group 1 (input.Trim() |> Seq.toList)
