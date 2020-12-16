module Program

let myCheck (f : int -> int -> bool) =
    let rand = System.Random()
    for _ in 1..100 do
        let x = rand.Next()
        let y = rand.Next()
        assert f x y

module internal Common =
    open System.Collections.Generic

    let memorizeWith (memo : IDictionary<'a, 'b>) (f : 'a -> 'b) (n : 'a) =
        lock memo (fun _ ->
            match memo.TryGetValue n with
            | true, res -> res
            | _ ->
                let res = f n
                memo.Add(n, res)
                res)

    let memorize (f : 'a -> 'b) =
        let t = new Dictionary<'a, 'b>()
        memorizeWith t f

    let (|MapContains|_|) = Map.tryFind

open Microsoft.FSharp.Reflection

module internal Reflect =
    open System

    let isRecordType (ty : Type) = FSharpType.IsRecord(ty, true)

    let isUnionType (ty: Type) = FSharpType.IsUnion(ty, true)

    let isTupleType (ty: Type) = FSharpType.IsTuple ty

type MyRecord = { MyRecordField: int }

type MyTuple = int * bool

type MyUnion = CA of int | CB of bool

open FsCheck

let revRevIsOrig (xs: int list) =
    xs |> List.rev |> List.rev = xs

let productOfTwoPosNumsIsPosNum (x: int) (y: int) =
    (x > 0 && y > 0) ==> (x * y > 0)

let listLength (xs: int list) (ys: int list) =
    List.length xs + List.length ys = List.length (xs @ ys)

// let tooEager a = a <> 0 ==> (1 / a = 1 / a) // throws DivideByZeroException
let moreLazy a =
    a <> 0 ==> lazy (1 / a = 1 / a)

let listAppend x =
    let arbList = Arb.from<int list>
    Prop.forAll arbList (fun xs -> List.rev (x::(List.rev xs)) = xs @ [x])

[<EntryPoint>]
let main argv =
    myCheck (fun x y -> x - y = (-1) * (y - x))

    printfn "%s is RecordType: %b" (nameof MyRecord) (Reflect.isRecordType typeof<MyRecord>)
    printfn "%s is UnionType: %b" (nameof MyUnion) (Reflect.isUnionType typeof<MyUnion>)
    printfn "%s is TupleType: %b" (nameof MyTuple) (Reflect.isTupleType typeof<MyTuple>)
    printfn "%s's constructors: %A" (nameof MyUnion) (FSharpType.GetUnionCases(typeof<MyUnion>, true))

    Check.Quick revRevIsOrig
    Check.Quick productOfTwoPosNumsIsPosNum
    Check.Quick listLength
    Check.Quick moreLazy
    Check.Quick listAppend
    0
