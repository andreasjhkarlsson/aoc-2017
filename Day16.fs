module Day16

open Common

type Dancers =
    { Arrangement: char[]; mutable Head: int }
    with
        static member create count =
            { Arrangement = Array.init count ((+) (int 'a') >> char); Head = 0 }

                
type Command = Spin of int | Exchange of int*int | Partner of char*char
   
[<Day(16, "Permutation Promenade")>]
let solve input =

    let dance =
    
        let commands =
            input
            |> split ","     
            |> Array.map (
                function
                | Regex @"s(\d+)" [s] -> Spin (int s)
                | Regex @"x(\d+)/(\d+)" [a; b] -> Exchange (int a, int b)
                | Regex @"p(\w)/(\w)" [a; b] -> Partner (char a, char b)
                | _ -> failwith "Invalid input"
            )

        fun dancers ->
            
            let indexOf position = (dancers.Head + position) % dancers.Arrangement.Length
            let arr = dancers.Arrangement
            let swap i1 i2 =
                let tmp = arr.[i1]
                arr.[i1] <- arr.[i2]
                arr.[i2] <- tmp

            for command in commands do
                match command with
                | Spin s ->
                    do dancers.Head <- indexOf (arr.Length - s)
                | Exchange (p1, p2) ->
                    do swap (indexOf p1) (indexOf p2)
                | Partner (d1, d2) ->
                    do swap (Array.findIndex ((=) d1) arr) (Array.findIndex ((=) d2) arr)

            dancers

    let lineup { Arrangement = arr; Head = head } =
        Seq.init arr.Length id
        |> Seq.map (fun i ->
            arr.[(head + i) % arr.Length] |> string
        )
        |> String.concat ""        
    
    let part1 = Dancers.create 16 |> dance |> lineup

    let part2 = 
        let dancers = Dancers.create 16

        let mutable seen = Map.empty<string, int>

        Seq.initInfinite (fun i -> i, if i = 0 then lineup dancers else  dance dancers |> lineup)
        |> Seq.find (fun (i, lu) ->
            if seen.ContainsKey(lu) then true
            else
                do seen <- seen |> Map.add lu i
                false
        ) |> ignore

        seen
        |> Map.findKey (fun _ i -> i = 1000000000 % seen.Count)


    { Part1 = part1; Part2 = part2 }