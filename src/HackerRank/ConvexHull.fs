module ConvexHull
open System
open System.Diagnostics

[<DebuggerDisplay("X={X}; Y={Y}")>]
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
        let newKontur = match kontur with
                        | a::b::remains -> if predicate a b c then c::kontur else c::b::remains
                        | _ -> c::kontur
        processVector predicate newKontur tail
    | _ -> kontur

let rec calcPerimeter sum vectors =
    match vectors with
    | a::b::tail -> calcPerimeter (sum + (calcDistance a b)) (b::tail)
    | _ -> sum

let solve problem =
    let sortedVectors = problem.Vectors 
                        |> List.sortWith (fun v1 v2 -> match v1.X > v2.X || v1.X = v2.X && v1.Y > v2.Y with
                                                       | true -> 1
                                                       | false -> -1)
    let a = sortedVectors |> List.head
    let b = sortedVectors |> List.last

    let firstPart = sortedVectors |> List.filter (area a b >> (<=) 0) 
    let secondPart = sortedVectors |> List.filter (area a b >> (>=) 0)

    let initialKontur = []
    let firstOuter = processVector isCw initialKontur firstPart
    let secondOuter = processVector isCcw initialKontur secondPart
    let lp = (calcPerimeter 0.0 firstOuter)
    let rp = (calcPerimeter 0.0 secondOuter)
    lp + rp

[<EntryPoint>]
let main argv = 
    let problem = parseProblem
    let solution = solve problem
    printfn "%f" solution
    0 // return an integer exit code

