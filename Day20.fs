module Day20

open Common

type Vec3<'a> =
    Vec3 of 'a*'a*'a

// Add vectors
let inline (+|+) (Vec3 (x1,y1,z1)) (Vec3 (x2,y2,z2)) = Vec3 (x1+x2,y1+y2,z1+z2)

// Manhattan distance between vectors
let inline (|-|) (Vec3 (x1,y1,z1)) (Vec3 (x2,y2,z2)) = (abs (x1-x2)) + (abs (y1-y2)) + (abs (z1-z2))

// Compare vectors
let inline (=|=) (Vec3 (x1,y1,z1)) (Vec3 (x2,y2,z2)) = x1 = x2 && y1 = y2 && z1 = z2

type Particle = { Id: int; Position: Vec3<int64>; Speed: Vec3<int64>; Acceleration: Vec3<int64> }

let step particle = {
            particle with
                Position = particle.Position +|+ particle.Speed +|+ particle.Acceleration
                Speed = particle.Speed +|+ particle.Acceleration
        }

let rec simulateMany particles = seq {
    yield particles
    yield! particles |> List.map step |> simulateMany
}

[<Day(20, "Particle Swarm")>]
let solve (input: string) =
    
    let particles =
        input
        |> parseLines
        |> Array.mapi (fun n line ->
            match line with
            | Regex @"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>"
                [px; py; pz; vx; vy; vz; ax; ay; az] ->
                    { Id = n; Position = Vec3 (int64 px, int64 py, int64 pz); Speed = Vec3 (int64 vx, int64 vy, int64 vz); Acceleration = Vec3 (int64 ax, int64 ay, int64 az)}
            | _ -> failwith "Bad input"
        )
        |> Array.toList

    let part1 =
        particles
        |> simulateMany
        |> Seq.take 500
        |> Seq.last
        |> List.minBy (fun p -> p.Position |-| (Vec3(0L, 0L, 0L)))
        |> fun p -> p.Id

    let part2 =
        {0..500}
        |> Seq.fold (fun particles _ ->
            particles
            |> List.filter (fun p ->
                particles
                |> List.exists (fun p2 -> p.Id <> p2.Id && p.Position =|= p2.Position)
                |> not
            )
            |> List.map step
        ) particles
        |> List.length

    { Part1 = part1; Part2 = part2}