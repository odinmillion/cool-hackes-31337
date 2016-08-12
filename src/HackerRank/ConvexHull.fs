open System
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Vector = { x : int; y: int }

let getVector (getVectorString:(unit -> string)) =
    let arr = getVectorString().Split(' ') |> Array.map int
    {x = arr.[0]; y = arr.[1]}

let area a b c = (b.x - a.x)*(c.y - a.y) - (b.y-a.y)*(c.x-a.x)

let isCw a b c = area a b c > 0

let isCcw a b c = not (isCw a b c)

let rec processVector predicate kontur candidates =
    match candidates with
    | c::tail -> 
        let b::a::remains = kontur
        let newKontur = if predicate a b c then c::kontur else c::a::remains
        processVector predicate newKontur tail
    | _ -> kontur

let calcDistance a b = Math.Sqrt ((double)((a.x - b.x)*(a.x - b.x) + (a.y - b.y)*(a.y - b.y)))

let rec calcPerimeter sum e vectors =
    match vectors with
    | c::[b;a]-> sum + (calcDistance c a) + (calcDistance b e)
    | a::b::tail -> calcPerimeter (sum + (calcDistance a b)) e (b::tail)
    | _ -> sum

let rec fold2 func acc list =
    match list with
    | a::b::tail -> fold2 func (func acc a b) (b::tail)
    | _ -> acc

[<EntryPoint>]
let main argv = 
    let n = Console.ReadLine() |> int
    let vectors = [1..n] |> List.map (fun _ -> getVector Console.ReadLine)
    let sortedVectors = vectors |> List.sortWith (fun v1 v2 -> match v1.x > v2.x || v1.x = v2.x && v1.y > v2.y with
                                                               | true -> 1
                                                               | false -> -1)

    if n = 3 
    then
        printfn "%f" (fold2 (fun acc a b -> acc + calcDistance a b) 0.0 (sortedVectors @ [List.head sortedVectors]))
    else
        let a = sortedVectors |> List.head
        let b = sortedVectors |> List.last

        let firstPart = sortedVectors |> List.filter (area a b >> (<) 0)
        let secondPart = sortedVectors |> List.filter (area a b >> (>) 0)

        let initialKontur = [b; a]
        let firstOuter = processVector isCw initialKontur firstPart
        let secondOuter = processVector isCcw initialKontur secondPart
        let lp = (calcPerimeter 0.0 firstOuter.Head firstOuter)
        let rp = (calcPerimeter 0.0 secondOuter.Head secondOuter)
        printfn "%f" (lp + rp)
    |> ignore
    0 // return an integer exit code

