module Day7

open Common

type Tower =
    { Name: string; Weight: int; Stacks: Tower list }
    with
        member x.TotalWeight =
            x.Weight + (x.Stacks |> List.sumBy (fun s -> s.TotalWeight))
        member x.Balanced =
            x.Stacks |> Seq.ofList |> Seq.distinctBy(fun s -> s.TotalWeight) |> Seq.length |> ((=) 1)

        member x.FindBalance shouldWeigh =

            if x.TotalWeight <> shouldWeigh && x.Balanced then
                x.Weight + (shouldWeigh - x.TotalWeight)
            elif not x.Balanced then
                let children =
                    x.Stacks
                    |> List.map (fun s ->
                        s,
                        x.Stacks
                        |> List.exists(fun ss -> s.Name <> ss.Name && ss.TotalWeight = s.TotalWeight))
                // What *should* all the children weigh?
                let childrenShouldWeigh = (children |> List.find snd |> fst).TotalWeight

                // Which child doesn't
                let unbalancedChild = children |> List.find (snd >> not) |> fst

                unbalancedChild.FindBalance childrenShouldWeigh
            else x.Weight

let findRoot stubs =
    stubs |> Array.map (fun (name,_,_) ->
        name,
        stubs |> Array.sumBy (fun (_,_,children) ->
            if children |> Array.exists ((=) name) then 1 else 0
        )
    )
    |> Array.sortBy snd
    |> Array.toList
    |> List.head
    |> fst

let rec buildTower stubs rootName =
    let name, weight, children = stubs |> Array.find (fun (name,_,_) -> rootName = name)
    {
        Name = name;
        Weight = weight;
        Stacks = children |> List.ofArray |> List.map (buildTower stubs)
    }


[<Day(7)>]
let solve input=

    // Parse into tower stubs (flat structure)
    let stubs =
        parseLines input
        |> Array.map (
            function
            | Regex @"([a-z]+) \((\d+)\)(?: -> )?(.*)" [name; weight; childList] ->
                name,
                int weight,
                childList.Split([|','|]) |> Array.choose(fun s -> match s.Trim() with "" -> None | trimmed -> Some trimmed)
            | _ -> failwith "Invalid input"
        )

    let tower = buildTower stubs (findRoot stubs)

    let balance = (tower.FindBalance tower.TotalWeight)

    {Part1 = tower.Name; Part2 = balance}

