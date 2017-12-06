module Common

open System.Reflection
open System
open System.IO
open System.Text

type Solution<'a,'b> =
    {
        Part1: 'a
        Part2: 'b
    }

type DayAttribute (day) =
    inherit System.Attribute ()

    member x.Day: int = day

    with
        static member All =
            Assembly.GetExecutingAssembly().GetTypes() 
            |> Array.collect (fun typ -> typ.GetMethods())
            |> Array.choose (fun mi -> 
                let attr = mi.GetCustomAttribute(typeof<DayAttribute>)
                if attr <> null then Some (attr :?> DayAttribute, mi)
                else None)
            |> Array.sortBy (fun (d, _) -> d.Day)
            |> Array.toList


type Console with
    static member ReadAll () =
        use stdin = Console.OpenStandardInput ()
        use reader = new StreamReader(stdin, Encoding.UTF8)
        reader.ReadToEnd ()  
