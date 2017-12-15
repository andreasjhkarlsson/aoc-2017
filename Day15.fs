module Day15

open Common



[<Day(15, "Dueling Generators")>]
let solve (input: string) =

    let judge count s1 s2 =
        Seq.zip s1 s2
        |> Seq.take count
        |> Seq.sumBy (fun (a,b) ->
            if (a&&&0xFFFFUL) = (b&&&0xFFFFUL) then 1 else 0
        )
    
    let generator (factor: uint64) =
        let rec generate (previous: uint64) =
            seq {
                let value = uint64 <| (bigint previous) * (bigint factor)  % (bigint 2147483647UL)
                yield value
                yield! generate value
            }
        generate

    let ai, bi = let splitted = input.Trim().Split([|' '|]) in uint64 splitted.[0], uint64 splitted.[1]

    let generatorA = generator 16807UL ai
    let generatorB = generator 48271UL bi

    let part1 =
        (generatorA, generatorB)
        ||> judge 40000000

    let part2 =
        ((generatorA |> Seq.filter (fun n -> n%4UL = 0UL)),
        (generatorB |> Seq.filter (fun n -> n%8UL = 0UL)))
        ||> judge 5000000

    { Part1 = part1; Part2 = part2 }