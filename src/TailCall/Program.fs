open System.Diagnostics

let rec fact n = if n > 0 then n * (fact (n - 1)) else 1

let rec factCPS n k =
    if n > 0
    then k 1
    else factCPS (n - 1) (fun x -> k (n * x))

[<EntryPoint>]
let main argv =
    let stopWatch = Stopwatch()
    stopWatch.Start()
    for i = 1 to 10000 do
        // ignore <| fact 1000
        ignore <| factCPS 1000 id
    stopWatch.Stop()
    let ts = stopWatch.Elapsed
    printfn $"Elapsed Time: %f{ts.TotalMilliseconds} ms"
    0
