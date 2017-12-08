module Day8

open Common

type Operation = Inc | Dec

type Comparison = Comparison of string
    with
        static member Compare left (Comparison comparison) right =
            match comparison with
            | "<" -> left < right
            | "<=" -> left <= right
            | ">" -> left > right
            | ">=" -> left >= right
            | "==" -> left = right
            | "!=" -> left <> right
            | _-> failwith "Invalid operator"

type Instruction =
    { Register: string; Operation: Operation; Operand: int; Condition: string*Comparison*int }
    with
        static member Execute registers (instruction: Instruction) =
            // Lookup register value from map
            let getRegister register = registers |> Map.tryFind register|> Option.defaultValue 0

            let conditionalRegister, comparison, conditionalValue = instruction.Condition

            // Check condition
            if Comparison.Compare (getRegister conditionalRegister) comparison conditionalValue then
                let newRegValue = // Calculate new register value
                    if instruction.Operation = Inc then (getRegister instruction.Register) + instruction.Operand
                    else (getRegister instruction.Register) - instruction.Operand

                // Add new value to map
                registers |> Map.add instruction.Register newRegValue
            else registers

[<Day(8,"I Heard You Like Registers")>]
let solve input =

    let highestValue = Map.toList >> List.map snd >> List.max

    let finalState, finalHighestValue =
        input
        |> parseLines
        |> Array.map (fun line ->
            // Parse line into instruction
            let tokens = parseList id line
            {
                Register = tokens.[0]
                Operation = if tokens.[1] = "inc" then Inc else Dec
                Operand = int tokens.[2]
                Condition = tokens.[4], Comparison tokens.[5], int tokens.[6]
            }
        )
        |> Array.fold (fun (registers, maxValue) instruction ->
            // Step 'cpu'
            let newRegisters = instruction |> Instruction.Execute registers
            let newMaxValue = highestValue newRegisters

            // New fold state
            newRegisters,
            if newMaxValue > maxValue then newMaxValue else maxValue
        ) (Map.empty, 0)

    { Part1 = highestValue finalState; Part2 = finalHighestValue }