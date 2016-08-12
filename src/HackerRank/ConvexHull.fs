module ConvexHull
open System

type Vector = { X : int; Y: int }

type Problem = { Vectors : Vector list }

let getVector (getVectorString:(unit -> string)) =
    let arr = getVectorString().Split(' ') |> Array.map int
    {X = arr.[0]; Y = arr.[1]}

let parseProblem = 
    let n = Console.ReadLine() |> int
    let vectors = [1..n] |> List.map (fun _ -> getVector Console.ReadLine)
    { Vectors = vectors }

let calcDistance a b = Math.Sqrt ((double)((a.X - b.X)*(a.X - b.X) + (a.Y - b.Y)*(a.Y - b.Y)))

let area a b c = (b.X - a.X)*(c.Y - a.Y) - (b.Y-a.Y)*(c.X-a.X)

let isCw a b c = area a b c > 0

let isCcw a b c = not (isCw a b c)

let rec processVector predicate kontur candidates =
    match candidates with
    | c::tail -> 
        let b::a::remains = kontur
        let newKontur = if predicate a b c then c::kontur else c::a::remains
        processVector predicate newKontur tail
    | _ -> kontur

let rec calcPerimeter sum e vectors =
    match vectors with
    | c::[b;a]-> sum + (calcDistance c a) + (calcDistance b e)
    | a::b::tail -> calcPerimeter (sum + (calcDistance a b)) e (b::tail)
    | _ -> sum

let rec fold2 func acc list =
    match list with
    | a::b::tail -> fold2 func (func acc a b) (b::tail)
    | _ -> acc

let solve problem =
    let sortedVectors = problem.Vectors 
                        |> List.sortWith (fun v1 v2 -> match v1.X > v2.X || v1.X = v2.X && v1.Y > v2.Y with
                                                       | true -> 1
                                                       | false -> -1)
    if problem.Vectors.Length = 3 
    then
        fold2 (fun acc a b -> acc + calcDistance a b) 0.0 (sortedVectors @ [List.head sortedVectors])
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
        lp + rp

[<EntryPoint>]
let main argv = 
    let problem = parseProblem
    let solution = solve problem
    printfn "%f" solution
    0 // return an integer exit code

