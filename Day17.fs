module Day17

open Common

[<Day(17, "Spinlock")>]
let solve (input: string) =
    
    let input = int input

    let spinlock times fn state =
            Seq.init times id
            |> Seq.fold (fun (state,(length, position)) _ ->
                let next = ((position + input) % length) + 1
                (fn state next length), (length + 1, next)
            ) (state,(1, 0))           

    let part1 =
        let buffer, (_, position) =
            spinlock 2017 (fun buffer next value ->
                let pre, post = buffer |> List.splitAt next
                pre @ (value::post)
            ) [0]
        (Array.ofList buffer).[position + 1]

    let part2 =
        let result, _ =
            spinlock 50000000 (fun afterZero next value ->
                if next = 1 then value else afterZero
            ) 0
        result

    { Part1 = part1; Part2 = part2 }