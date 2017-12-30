module Day25

open Common
open System.Text.RegularExpressions

module Turing =

    let (?|?) = defaultArg 

    type Bit = Set | Clear

    type Direction = Left | Right

    type Tape = Map<int, Bit>

    type Machine = {
        Tape: Tape
        Cursor: int
        CurrentState: State
        States: Map<string, State>
    }
    with
        static member Read (machine: Machine) = (machine.Tape.TryFind machine.Cursor ) ?|? Clear
        static member Write value machine = {machine with Tape = machine.Tape |> Map.add machine.Cursor value }
        static member Enter state machine = {machine with CurrentState = machine.States.[state]}
        static member Move direction machine = {machine with Cursor = machine.Cursor + (match direction with Left -> -1 | Right -> 1) }
    and State = {
        Execute: Bit->(Machine->Machine)
    }

    let rec simulate (machine: Machine) = seq {
        let machine = machine.CurrentState.Execute (Machine.Read machine) machine
        yield machine
        yield! simulate machine
    }

    let checksum (machine: Machine) =
        machine.Tape
        |> Map.toList
        |> List.filter (snd >> ((=) Set))
        |> List.length

[<Day(25, "The Halting Problem")>]
let solve input = 

    let initialState =
        match input with
        | Regex @"Begin in state (\w+)" [initialState] -> initialState
        | _ -> failwith "Invalid input" 

    let steps = 
        match input with
        | Regex @"Perform a diagnostic checksum after (\d+) steps" [steps] -> int steps
        | _ -> failwith "Invalid input"

    // MOAR (Mother of all regexes)
    let stateRegex = @"(In state (\w+).*?If.*?(\d+).*?- (.*?)- (.*?)- (.*?)If.*?(\d+).*?- (.*?)- (.*?)- (.*?\.))"

    let states =
        [for m in Regex.Matches(input, stateRegex,RegexOptions.Singleline) -> m]
        |> List.map (fun m ->
            let strToBit str = if str = "0" then Turing.Clear else Turing.Set 

            let parseInstruction =
                function
                | Regex @"Write.*?(\w+)\." [v] -> Turing.Machine.Write (strToBit v)
                | Regex @"Move.*?(\w+)\." [d] -> Turing.Machine.Move (if d = "left" then Turing.Left else Turing.Right)
                | Regex @"Continue.*?(\w+)\." [s] -> Turing.Machine.Enter s
                | _ -> failwith "Invalid instruction"

            let instructions =
                [
                    (strToBit m.Groups.[3].Value,
                        parseInstruction (m.Groups.[4].Value)
                        >> parseInstruction (m.Groups.[5].Value) 
                        >> parseInstruction (m.Groups.[6].Value))
                    (strToBit m.Groups.[7].Value,
                        parseInstruction (m.Groups.[8].Value)
                        >> parseInstruction (m.Groups.[9].Value) 
                        >> parseInstruction (m.Groups.[10].Value))
                ] |> Map.ofList

            m.Groups.[2].Value, { Turing.State.Execute = fun bit -> instructions.[bit] }
        )
        |> Map.ofList
    
    let machine = {
        Turing.Machine.Cursor = 0
        Turing.Machine.States = states
        Turing.Machine.Tape = Map.empty
        Turing.Machine.CurrentState = states.[initialState]
    }
    
    let part1 =
        machine
        |> Turing.simulate 
        |> Seq.take steps
        |> Seq.last
        |> Turing.checksum

    { Part1 = part1; Part2 = Option<int>.None }