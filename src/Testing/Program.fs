module Program

let myCheck (f : int -> int -> bool) =
    let rand = System.Random()
    for _ in 1..100 do
        let x = rand.Next()
        let y = rand.Next()
        assert f x y

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
    Check.Quick revRevIsOrig
    Check.Quick productOfTwoPosNumsIsPosNum
    Check.Quick listLength
    Check.Quick moreLazy
    Check.Quick listAppend
    0
