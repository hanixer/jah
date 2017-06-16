#load "Language.fs"
#load "Util.fs"
#load "Gm.fs"

open Gm
open Language

compileSc ("K", ["x"; "y"], EVar "x")

compileSc ("S", ["f"; "g"; "x"], EAp (EAp (EVar "f", EVar "x"), EAp (EVar "g", EVar "x")))
"S", 3, compileR (EAp (EAp (EVar "f", EVar "x"), EAp (EVar "g", EVar "x")))  ["f", 0; "g", 1; "x", 2]
"S", 3, (compileC (EAp (EAp (EVar "f", EVar "x"), EAp (EVar "g", EVar "x")))  ["f", 0; "g", 1; "x", 2] |>List.append<| [ Slide 4; Unwind ])
"S", 3, (List.concat 
    [   compileC (EAp (EVar "f", EVar "x")) ["f", 0; "g", 1; "x", 2]; 
        compileC (EAp (EVar "g", EVar "x"))  ["f", 1; "g", 2; "x", 3]; 
        [Mkap] ] 
    |>List.append<| [ Slide 4; Unwind ])
"S", 3, (List.concat 
    [   List.concat
            [   compileC (EVar "f") ["f", 0; "g", 1; "x", 2];
                compileC (EVar "x") ["f", 1; "g", 2; "x", 3];
                [Mkap] ]
        
        compileC (EAp (EVar "g", EVar "x"))  ["f", 1; "g", 2; "x", 3]; 
        [Mkap] ] 
    |>List.append<| [ Slide 4; Unwind ])
"S", 3, (List.concat 
    [   [Pushint 0];
        [Pushint 4];
        [Mkap];
        [Pushint 3];
        [Pushint 5];
        [Mkap];
        [Mkap] ] 
    |>List.append<| [ Slide 4; Unwind ])
"S", 3, (
    [   Pushint 0
        Pushint 4
        Mkap
        Pushint 3
        Pushint 5
        Mkap
        Mkap ] 
    |>List.append<| [ Slide 4; Unwind ])
"S", 3,
    [   Pushint 0
        Pushint 3
        Mkap
        Pushint 2
        Pushint 4
        Mkap
        Mkap
        Slide 4; 
        Unwind ] 
let src = "id = S K K ;
main = twice twice twice id 3"

let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src  |> fprintf f "%s"

g src