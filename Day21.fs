module Day21

open Common


module Matrix =

    let jaggedTo2D (lst: 'a list list) =
        let m = Array2D.create lst.Length lst.Head.Length Unchecked.defaultof<'a>
        lst |> List.iteri (fun y r -> r |> List.iteri (fun x e -> m.[y,x] <- e))
        m

    let TwoXTwo ((a,b),(c,d)) = jaggedTo2D [[a;b];[c;d]] 
    let (|TwoXTwo|_|) m =
        if (Array2D.length1 m) = 2 then
            Some ((m.[0,0],m.[0,1]),(m.[1,0],m.[1,1]))
        else None
    let ThreeXThree ((a,b,c),(d,e,f),(g,h,i)) = jaggedTo2D [[a;b;c];[d;e;f];[g;h;i]]
    let (|ThreeXThree|_|) m =
        if (Array2D.length1 m) = 3 then
            let row y = m.[y,0], m.[y,1], m.[y,2]
            Some (row 0, row 1, row 2)
        else None

    let rotate =
        function
        | TwoXTwo ((a,b),(c,d)) -> TwoXTwo ((c,a),(d,b))
        | ThreeXThree ((a,b,c),(d,e,f),(g,h,i)) -> ThreeXThree ((g,d,a),(h,e,b),(i,f,c))
        | _ -> failwith "Unsupported"

    let rotations matrix = List.init 3 id |> List.scan (fun m _ -> rotate m) matrix

    let flipX =
        function
        | TwoXTwo (r0,r1) -> TwoXTwo (r1,r0)
        | ThreeXThree (r0,r1,r2) -> ThreeXThree (r2,r1,r0)
        | _ -> failwith "Unsupported"

    let flipY =
        function
        | TwoXTwo ((a,b),(c,d)) -> TwoXTwo ((b,a),(d,c))
        | ThreeXThree ((a,b,c),(d,e,f),(g,h,i)) -> ThreeXThree ((c,b,a), (f,e,d), (i,h,g))
        | _ -> failwith "Unsupported"

    let configurations matrix =
        [id; flipY; flipX]
        |> List.map (fun t -> rotations matrix |> List.map (fun r -> t r))
        |> List.concat

    let splitInto size (matrix: 'a[,]) =

        List.init ((Array2D.length1 matrix) / size) id
        |> List.map (fun y ->
            List.init ((Array2D.length2 matrix) / size) id
            |> List.map (fun x ->
                let dest = Array2D.zeroCreate size size
                Array2D.blit matrix (y*size) (x*size) dest 0 0 size size
                dest
            )
        )
        |> jaggedTo2D

    let join (matrices: 'a[,][,]) =
        let es = matrices.[0,0] |> Array2D.length1
        let width = es * (Array2D.length1 matrices)
        let result = Array2D.zeroCreate width width
        Seq.init (Array2D.length1 matrices) id 
        |> Seq.iter (fun y ->
            Seq.init (Array2D.length2 matrices) id
            |> Seq.iter (fun x ->
                do Array2D.blit matrices.[y,x] 0 0 result (y*es) (x*es) es es
            )
         )
        result

    let ofString =
        split "/"
        >> Array.map (fun s -> s.Trim())
        >> List.ofArray
        >> List.map (Seq.map string >> Seq.toList)
        >> jaggedTo2D

    let toString matrix =
        
        Seq.init (Array2D.length1 matrix) id
        |> Seq.map (fun y ->
            Seq.init (Array2D.length2 matrix) (fun x ->
                matrix.[y,x]
            )
            |> String.concat ""
        )
        |> String.concat "/"

    let sumBy (fn: 'a -> int) matrix =
        Seq.init (Array2D.length1 matrix) id
        |> Seq.map (fun i ->
            Seq.init (Array2D.length2 matrix) id
            |> Seq.map (fun j -> fn matrix.[i,j])
        )
        |> Seq.concat
        |> Seq.sum

[<Day(21, "Fractal Art")>]
let solve (input: string) =

    let rules =
        input 
        |> parseLines
        |> List.ofArray
        |> List.map (
            function
            | Regex @"(.*) => (.*)" [i;o] -> Matrix.ofString i, Matrix.ofString o
            | _ -> failwith "Bad input"
        )
        |> List.map (fun (i,o) -> Matrix.configurations i |> List.map (fun m -> m, o))
        |> List.concat
        |> Map.ofList

    let start = Matrix.ofString ".#./..#/###"

    let rec expand times (matrix: string[,]) =
        if times = 0 then matrix
        else
            let splitted =
                if (Array2D.length1 matrix) % 2 = 0 then Matrix.splitInto 2 matrix
                elif (Array2D.length2 matrix) % 3 = 0 then Matrix.splitInto 3 matrix
                else failwith "Something went wrong"

            splitted
            |> Array2D.map (fun m ->
                match rules |> Map.tryFind m with
                | Some out -> out
                | None ->
                    failwith "No matching rule for this pattern!"
            )
            |> Matrix.join
            |> expand (times - 1)

    let part1 = (start |> expand 5 |> Matrix.sumBy (fun s -> if s = "#" then 1 else 0))
    let part2 = (start |> expand 18 |> Matrix.sumBy (fun s -> if s = "#" then 1 else 0))
    
    { Part1 = part1; Part2 = part2 }