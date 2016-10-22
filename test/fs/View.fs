namespace Test

open Fable.Core.Testing
open Pauan.View

[<TestFixture>]
module View =
    [<Test>]
    let ``always |> value``() =
        equal "hi" (View.always "hi" |> View.value)
