module Day9

open Common

type Mode = Group | Garbage
type State = {Mode: Mode; Depth: int; Score: int; GC: int; Skip: bool}

[<Day(9,"Stream Processing")>]
let solve (input: string) =

    input.Trim()
    |> Seq.fold (fun ({Depth = depth; Score = score; GC = gc} as state) chr ->
        let inside = (=) state.Mode

        match chr with
        | _ when state.Skip -> {state with Skip = false}
        | '{' when inside Group -> {state with Depth = depth + 1; Score = score + depth}
        | '}' when inside Group -> {state with Depth = depth - 1}
        | '<' when inside Group -> {state with Mode = Garbage}
        | '>' when inside Garbage -> {state with Mode = Group}
        | '!'  -> {state with Skip = true}
        | _ when inside Garbage -> {state with GC = gc + 1}
        | _ -> state
    ) {Mode = Group; Depth = 1; Score = 0; GC = 0; Skip = false}
    |> fun {Score = score; GC = gc} -> {Part1 = score; Part2 = gc}
