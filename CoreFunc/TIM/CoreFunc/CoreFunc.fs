module CoreFunc
open CompileGC

let fac = "fac n = if (n==0) 1 (n * fac (n-1)) ;
main = fac 4"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    CompileGC.runProg src |> fprintf f "%s"

let h =
    [ 1, NAp (2, 3)
      2, NInd 10
      3, NData (5, [1;4;5])
      4, NNum 5
      5, NNum 88
      11, NNum 5
      12, NNum 8
      10, NNum 123] |> Map.ofList

let state = { Stack = [11]; Globals =["fun",12]; Dump = [[1]]; Stats = 0; Exception = None; Heap = h }

[<EntryPoint>]
let main argv =
    gc state
    0 // return an integer exit code
