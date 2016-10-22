namespace Test

open Fable.Core.Testing
open Pauan.View

[<TestFixture>]
module Mutable =
    [<Test>]
    let ``getValue``() =
        let a = Mutable.make 5
        equal 5 (Mutable.transaction {
            return! a |> Mutable.getValue
        } |> Mutable.runTransaction)


    [<Test>]
    let ``setValue``() =
        let a = Mutable.make 5
        equal 10 (Mutable.transaction {
            do! a |> Mutable.setValue 10
            return! a |> Mutable.getValue
        } |> Mutable.runTransaction)
