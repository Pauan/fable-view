module Pauan.View.Dispose

open System


type Dispose(fn) =
    interface IDisposable with
        member this.Dispose() =
            fn ()

let dispose fn = (new Dispose(fn) :> IDisposable)


let noop =
    { new IDisposable with
        member this.Dispose() = () }


let tryDispose (a: option<IDisposable>) =
    match a with
    | None ->
        ()
    | Some a ->
        a.Dispose()
