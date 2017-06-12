#load "Language.fs"
#load "Compile.fs"

open Language
open Compile

let src = "main = S K K 3"

let g () =
    use f = System.IO.File.CreateText("output.txt")
    runProg src |> fprintf f "%s"

g()