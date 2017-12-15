module Day13

open Common

[<Day(13, "Packet Scanners")>]
let solve input =
    
    let position range time =
        let n = time % (range * 2 - 2)
        n - (n / range) * (n - range + 1) * 2

    let isCaught range = (position range) >> ((=) 0)

    let layers =
        parseLines input
        |> Array.map (split ": ")
        |> Array.map (fun a -> int a.[0], int a.[1])

    let part1 = layers |> Array.sumBy (fun (d,r) -> if isCaught r d then d*r else 0)

    let part2 =
        Seq.initInfinite id
        |> Seq.find (fun offset -> not (Array.exists (fun (d,r) -> isCaught r (d+offset)) layers))
        
    { Part1 = part1; Part2 = part2 }