module Day10

open Common

let inline (+=) (x : byref<_>) y = x <- x + y

let reverse (array: 'a[]) start count =
    let swap i j =
        let i, j = i % array.Length, j % array.Length
        let tmp = array.[i] in array.[i] <- array.[j]; array.[j] <- tmp 

    for i = 0 to (count / 2) - 1 do
        swap (start+i) ((start+(count-1))-i)

let knotHashRound (position: byref<int>) (skip: byref<int>) (lengths: int array) array =
    for length in lengths do
        do reverse array position length
        &position += (length + skip)
        &skip += + 1

let knotHash (input: string) =
    let sparse = Array.init 256 id
    let lengths =
        Array.append
            (input.Trim() |> Seq.map int |> Seq.toArray)
            [|17; 31; 73; 47; 23|]

    let mutable skip = 0
    let mutable position = 0

    for i = 0 to 63 do
        knotHashRound &position &skip lengths sparse

    sparse
    |> Array.chunkBySize 16
    |> Array.map (Array.reduce (^^^))
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let part1 (input: string) =
    let hash = Array.init 256 id
    let mutable skip = 0
    let mutable position = 0
    let lengths = input.Trim().Split([|','|]) |> Array.map int

    do knotHashRound &position &skip lengths hash

    hash.[0] * hash.[1]

let part2 = knotHash

[<Day(10, "Knot Hash")>]
let solve (input: string) =
    { Part1 = part1 input; Part2 = part2 input}
