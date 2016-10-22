namespace Test

open Fable.Core.Testing

[<TestFixture>]
module Mutable =
    [<Test>]
    let ``message is correct``() =
        equal "Hello world!" Message.message
