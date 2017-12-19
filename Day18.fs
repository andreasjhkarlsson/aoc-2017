module Day18

open Common

let (<?>) option ``default`` = match option with Some v -> v | None -> ``default``

type Value = int64
type Register = string
type Operand = Register of Register | Value of Value

module Inbox =

    type Message = In of int64 | Out of AsyncReplyChannel<int64>
    type Agent = MailboxProcessor<Message>

    let create () = Agent.Start (fun inbox -> 
            let rec run () = async {
                let! incoming = inbox.Scan(fun msg -> match msg with | In data -> Some (async { return data}) | _ -> None)
                let! outgoing = inbox.Scan(fun msg -> match msg with | Out reply -> Some (async { return reply}) | _ -> None)
                do outgoing.Reply incoming
                return! run ()
            }
            run ()
        )

    type Pipe =
        { In: Agent; Out: Agent }
        with
            member x.Send value = x.Out.Post (In value)
            member x.Receive () = x.In.PostAndReply(Out, 500)
            static member Dummy = { In = create(); Out = create()}
    
    let connect ``in`` out = {In = ``in``; Out = out}

type Instruction =
    | Snd of Operand
    | Set of Register * Operand
    | Add of Register * Operand
    | Mul of Register * Operand
    | Mod of Register * Operand
    | Rcv of Register
    | Jgz of Operand * Operand

type State = {
        Registers: Map<Register, Value>
        LastSound: Value
        SendCount: int64
        Recovered: Value
        PC: int64
        Id: int64
        Inbox: Inbox.Agent
        Terminated: bool
    }
    with
        member x.Advance offset = { x with PC = x.PC + offset}
        static member Create id = {  
                Registers = Map.ofList ["p", id]
                LastSound = 0L
                Recovered = 0L
                PC = 0L
                Id = id
                SendCount = 0L
                Inbox = Inbox.create ()
                Terminated = false
            }

[<Day(18, "Duet")>]
let solve input =

    let instructions =

        let operand op = try Value (int64 op) with | _ -> Register op 

        input
        |> parseLines
        |> Array.map (
            function
            | Regex @"snd (\S+)"        [op]        ->  Snd (operand op)
            | Regex @"set (\S+) (\S+)"  [reg; op]   ->  Set(reg, operand op)
            | Regex @"add (\S+) (\S+)"  [reg; op]   ->  Add(reg, operand op)
            | Regex @"mul (\S+) (\S+)"  [reg; op]   ->  Mul(reg, operand op)
            | Regex @"mod (\S+) (\S+)"  [reg; op]   ->  Mod(reg, operand op)
            | Regex @"rcv (\S+)"        [reg]       ->  Rcv reg
            | Regex @"jgz (\S+) (\S+)"  [op1; op2]  ->  Jgz (operand op1, operand op2)
            | _ -> failwith "Invalid input"
        )

    let part1 = 
        Seq.initInfinite id
        |> Seq.scan (fun state _ ->
                let (!) name = (state.Registers |> Map.tryFind name) <?> 0L
                let (~%) = function Register reg -> !reg | Value v -> v
                let (<!-) reg value = state.Registers |> Map.add reg value

                match instructions.[int state.PC] with
                | Snd op            ->  {state.Advance 1L with LastSound = %op}
                | Set (reg, op)     ->  {state.Advance 1L with Registers = reg <!- %op}
                | Add (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg + %op)}
                | Mul (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg * %op)}
                | Mod (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg % %op)}
                | Rcv op            ->  {state.Advance 1L with Recovered = if !op <> 0L then state.LastSound else state.Recovered}
                | Jgz (op1, op2)    ->  {state with PC = if %op1 > 0L then state.PC + (%op2) else state.PC + 1L}
        ) (State.Create 0L)
        |> Seq.find (fun s -> s.Recovered <> 0L)
        |> (fun s ->s.Recovered)

    let part2 =

        let rec run state (pipe: Inbox.Pipe) = 
            let (!) name = (state.Registers |> Map.tryFind name) <?> 0L
            let (~%) = function Register reg -> !reg | Value v -> v
            let (<!-) reg value = state.Registers |> Map.add reg value

            let newState =
                if state.PC < 0L || (int state.PC) >= instructions.Length then { state with Terminated = true}
                else
                    match instructions.[int state.PC] with
                    | Snd op            ->
                        do pipe.Send %op
                        {state.Advance 1L with SendCount = state.SendCount + 1L}
                    | Set (reg, op)     ->  {state.Advance 1L with Registers = reg <!- %op}
                    | Add (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg + %op)}
                    | Mul (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg * %op)}
                    | Mod (reg, op)     ->  {state.Advance 1L with Registers = reg <!- (!reg % %op)}
                    | Rcv reg           -> 
                        try
                            {state.Advance 1L with  Registers = reg <!- pipe.Receive ()}
                        with | :? System.TimeoutException -> { state with Terminated = true } // Deadlock
                    | Jgz (op1, op2)    ->  {state with PC = if %op1 > 0L then state.PC + (%op2) else state.PC + 1L}           
                             
            if newState.Terminated then newState
            else run newState pipe

        let program0 = State.Create 0L
        let program1 = State.Create 1L

        [ async { return run program0 (Inbox.connect program0.Inbox program1.Inbox) }
          async { return run program1 (Inbox.connect program1.Inbox program0.Inbox) } ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.find (fun s -> s.Id = 1L)
        |> fun s -> s.SendCount

    { Part1 = part1; Part2 = part2 }