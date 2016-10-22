module Pauan.View.Mutable

open Pauan.View.View


type Mutable<'A>(value) =
    inherit View<'A>()

    let mutable value = value

    let mutable id = 0

    let events = Event<TransactionId>()


    override this.SubscribeView fn =
        events.Publish.Subscribe fn

    // TODO is this correct ?
    override this.UpdateView _ =
        true
        //id

    override this.ValueView = value


    member internal this.Value
        with get() = value
        and set v = value <- v

    member internal this.Trigger id =
        //id <- id + 1
        id |> events.Trigger

let make a = Mutable(a)


[<NoComparison>]
[<NoEquality>]
type Changed = internal {
    Rollback: unit -> unit
    Commit: TransactionId -> unit
}

[<NoComparison>]
[<NoEquality>]
type Transaction<'A> =
    | Transaction of (ResizeArray<Changed> -> 'A)

let getValue (m: Mutable<'A>) =
    Transaction <| fun _ ->
        m.Value

let setValue (value: 'A) (m: Mutable<'A>) =
    Transaction <| fun state ->
        let oldValue = m.Value

        state.Add {
            Rollback = fun _ -> m.Value <- oldValue
            Commit = m.Trigger
        }

        m.Value <- value


type TransactionBuilder() =
    member this.Bind(Transaction a, f) =
        Transaction <| fun state ->
            match a state |> f with
            | Transaction m ->
                m state

    // TODO is this correct ?
    member this.Combine(Transaction a, Transaction b) =
        Transaction <| fun state ->
            a state
            b state

    member this.Return(a) =
        Transaction <| fun _ ->
            a

    member this.ReturnFrom(a) =
        a

    member this.Zero() =
        this.Return(())

// TODO test this
let transaction = TransactionBuilder()


let runTransaction (Transaction a) =
    let state = ResizeArray()

    try
        let value = a state

        let id = newTransactionId ()
        for f in state do
            f.Commit id

        value
    with
    // TODO test this
    | _ ->
        // TODO make this more efficient ?
        state.Reverse()
        for f in state do
            f.Rollback ()
        reraise ()
