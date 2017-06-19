#load "Language.fs"
#load "Util.fs"
#load "Gm.fs"

open Gm
open Language
let src = "id = S K K ;
main = twice twice twice id 3"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src  |> fprintf f "%s"

g src