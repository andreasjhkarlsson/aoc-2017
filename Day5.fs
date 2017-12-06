module Day5

open Common


let follow (jumps: int array) transform =
    
    let rec jump (jumps: int array) index steps =
        if index >= jumps.Length then steps
        else
            let next = jumps.[index]
            jumps.[index] <- transform next
            jump jumps (index + next) (steps + 1)

    jump (jumps.Clone() :?> int array) 0 0

let solvePart1 jumps = follow jumps ((+) 1)

let solvePart2 jumps = follow jumps (function n when n  >= 3 -> n - 1 | n -> n + 1)


[<Day(5)>]
let solve input =
    let jumps = parseList int input

    { Part1 = solvePart1 jumps; Part2 = solvePart2 jumps }
