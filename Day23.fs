module Day23

open Common
open Day18


type State =
    { PC: int64; Registers: Map<Register, int64>; Multiplications: int }
    with
        member x.Advance o = {x with PC = x.PC + o }

[<Day(23, "Coprocessor Conflagration")>]
let solve input =
    
    let instructions =
        input
        |> parseLines
        |> Array.map Instruction.OfString

    let rec execute (state: State) = seq {

        let (!) name = (state.Registers |> Map.tryFind name) <?> 0L
        let (~%) = function Register reg -> !reg | Value v -> v
        let (<!-) reg value = state.Registers |> Map.add reg value

        if state.PC < 0L || state.PC >= (int64 instructions.Length) then ()
        else
            let state = 
                match instructions.[int state.PC] with
                | Set (reg, op)     ->  {state.Advance 1L with Registers = reg <!- %op}
                | Sub (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg - %op)}
                | Mul (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg * %op); Multiplications = state.Multiplications + 1}
                | Jnz (op1, op2)    ->  {state with PC = if %op1 <> 0L then state.PC + (%op2) else state.PC + 1L}
                | _ -> failwith "Not supported in this puzzle"
            yield state
            yield! execute state
        }

    let part1 =
        { PC = 0L; Registers = Map.empty; Multiplications = 0 }
        |> execute
        |> Seq.last
        |> fun s -> s.Multiplications
    
    let part2 =
        // Run the input for a limited number of iterations to get initial values for b & c registers
        let startingState =
            { PC = 0L; Registers = Map.ofList ["a", 1L]; Multiplications = 0 }
            |> execute
            |> Seq.take 8
            |> Seq.last

        let bi = startingState.Registers.["b"] |> int
        let c = startingState.Registers.["c"] |> int

        {bi..17..c}
        |> Seq.filter (fun b ->
            {2..(b-1)}
            |> Seq.exists (fun d ->
                if (b%d) <> 0 then false
                else
                    {2..(b-1)} |> Seq.exists (fun e -> (d*e) = b)
            )
        )
        |> Seq.length
        
    { Part1 = part1; Part2 = part2 }
