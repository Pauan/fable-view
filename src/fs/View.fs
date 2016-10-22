module Pauan.View.View

open System
open Pauan.View.Dispose
open Fable.Core


[<Measure>]
type private Id

type TransactionId = int<Id>

let mutable private globalTransactionId = 0<Id>

let newTransactionId () =
    globalTransactionId <- globalTransactionId + 1<Id>
    globalTransactionId


type Changed = bool

[<AbstractClass>]
type View<'A>() =
    // TODO this should accept anything which implements `IDisposable`, rather than `IDisposable` itself
    abstract member SubscribeView : (TransactionId -> unit) -> IDisposable
    abstract member UpdateView : TransactionId -> Changed
    abstract member ValueView : 'A

    // TODO don't push updates if the new value is the same as the old value
    interface IObservable<'A> with
        member this.Subscribe out =
            // TODO store this in the object rather than in here ?
            let mutable transactionId = globalTransactionId
            // TODO should this subscribe before or after pushing ?
            let disposer = this.SubscribeView <| fun id ->
                if transactionId < id then
                    transactionId <- id
                    if this.UpdateView id then
                        out.OnNext this.ValueView
            // TODO should this update it before pushing ?
            out.OnNext this.ValueView
            disposer


type Map<'A>(f, parent: View<'A>) =
    inherit View<'A>()

    let mutable transactionId = globalTransactionId
    let mutable value = f parent.ValueView

    override this.ValueView =
        value

    override this.UpdateView id =
        if transactionId < id then
            transactionId <- id
            if parent.UpdateView id then
                value <- f parent.ValueView
                true
            else
                false
        else
            false

    override this.SubscribeView fn =
        parent.SubscribeView fn

let map f parent = Map<'A>(f, parent)


type Map2<'A, 'B>(f, parent1: View<'A>, parent2: View<'B>) =
    inherit View<'A>()

    let mutable transactionId = globalTransactionId
    let mutable value = f parent1.ValueView parent2.ValueView

    override this.ValueView =
        value

    override this.UpdateView id =
        if transactionId < id then
            transactionId <- id

            // Cannot use short-circuit || because it must always update both views
            let changed1 = parent1.UpdateView id
            let changed2 = parent2.UpdateView id

            if changed1 || changed2 then
                value <- f parent1.ValueView parent2.ValueView
                true
            else
                false
        else
            false

    // TODO figure out a better way of implementing this
    override this.SubscribeView fn =
        let stop1 = parent1.SubscribeView fn
        let stop2 = parent2.SubscribeView fn
        dispose <| fun _ ->
            stop1.Dispose()
            stop2.Dispose()

let map2 f parent1 parent2 = Map2<'A, 'B>(f, parent1, parent2)


type Filter<'A>(f, init, parent: View<'A>) =
    inherit View<'A>()

    let mutable transactionId = globalTransactionId

    let parentValue = parent.ValueView
    let mutable value = if f parentValue then parentValue else init

    override this.ValueView =
        value

    override this.UpdateView id =
        if transactionId < id then
            transactionId <- id
            if parent.UpdateView id then
                let parentValue = parent.ValueView
                if f parentValue then
                    value <- parentValue
                    true
                else
                    false
            else
                false
        else
            false

    override this.SubscribeView fn =
        parent.SubscribeView fn

let filter f init parent = Filter<'A>(f, init, parent)


// TODO test this
type Flatten<'A>(parent: View<View<'A>>) =
    inherit View<'A>()

    let mutable transactionId = globalTransactionId
    let mutable value = parent.ValueView.ValueView

    override this.ValueView =
        value

    override this.UpdateView id =
        if transactionId < id then
            transactionId <- id
            // TODO is this correct ? should this always update the child view ?
            if parent.UpdateView id || parent.ValueView.UpdateView id then
                value <- parent.ValueView.ValueView
                true
            else
                false
        else
            false

    override this.SubscribeView fn =
        let mutable stopChild = None

        // TODO is this correct ?
        let stopParent = parent.SubscribeView <| fun id ->
            let oldChild = stopChild

            stopChild <- Some <| parent.ValueView.SubscribeView fn

            tryDispose oldChild

            fn id

        dispose <| fun _ ->
            stopParent.Dispose()
            tryDispose stopChild

let flatten parent = Flatten<'A>(parent)


// TODO test this
// TODO this is probably wrong
type Async<'A>(init: 'A, parent: View<FSharp.Control.Async<'A>>) =
    inherit View<'A>()

    let mutable transactionId = globalTransactionId
    let mutable value = init

    let mutable counter = 0
    let mutable max = 0

    let updateView a =
        counter <- counter + 1
        let id = counter
        async {
            let! a = a
            if max < id then
                max <- id
                value <- a
        } |> Async.StartImmediate

    do
        updateView parent.ValueView

    override this.ValueView =
        value

    override this.UpdateView id =
        if transactionId < id then
            transactionId <- id
            if parent.UpdateView id then
                updateView parent.ValueView
                true
            else
                false
        else
            false

    override this.SubscribeView fn =
        parent.SubscribeView fn

let async init parent = Async<'A>(init, parent)


type Always<'A>(value) =
    inherit View<'A>()

    override this.ValueView = value

    override this.UpdateView _ = false

    override this.SubscribeView _ = noop

let always value = Always<'A>(value)


let value (a: View<'A>) =
    // TODO is this correct ?
    a.UpdateView globalTransactionId |> ignore
    a.ValueView
