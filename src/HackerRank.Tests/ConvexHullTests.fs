module ConvexHullTests

open NUnit.Framework
open ConvexHull
open FluentAssertions

[<TestFixture>]
type ConvexHullTests() =

    [<Test>]
    member this.SolveProblem1() =
        let problem = { Vectors=[{X=1;Y=1}; {X=2;Y=5}; {X=3;Y=3}; {X=5;Y=3}; {X=3;Y=2}; {X=2;Y=2}] }
        let solution = solve problem
        solution.Should().BeApproximately(12.2, 0.2, "", []) |> ignore
        
    [<Test>]
    member this.SolveProblem2() =
        let problem = { Vectors=[{X=3;Y=2}; {X=2;Y=5}; {X=4;Y=5}] }
        let solution = solve problem
        solution.Should().BeApproximately(8.3, 0.2, "", []) |> ignore

    [<Test>]
    member this.SolveProblem3() =
        let problem = { Vectors=[{X=0;Y=0}; {X=10;Y=10}; {X=0;Y=10}] }
        let solution = solve problem
        solution.Should().BeApproximately(34.14, 0.2, "", []) |> ignore
    
    [<Test>]
    member this.CalcDistance1() = 
        Assert.AreEqual(1, calcDistance {X=0; Y=0} {X=1; Y=0})
    
    [<Test>]
    member this.CalcDistance2() = 
        Assert.AreEqual(0, calcDistance {X=0; Y=0} {X=0; Y=0})
