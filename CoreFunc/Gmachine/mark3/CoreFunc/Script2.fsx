#load "Language.fs"
#load "Util.fs"
#load "Gm.fs"

open Gm
open Language
let src = "id = S K K ;
main = twice twice twice id 3"
let src2 = "twiced f x = f (f x);
id x = x;
apply f x = f x;
constant x = 5;
main = twice twice twice id 3"
let src3 = "f1 x = 1;
g2 x = 2;
main = compose f1 g2 3"
let src4 = "id = S K K ;
main = twice twice id 3"
let src5 = "f x = letrec y = I z; z = I 3 in K x z;
main = f 1"


let g src =
    use f = System.IO.File.CreateText("output.txt")
    runProg src  |> fprintf f "%s"

let h src =
    use f = System.IO.File.CreateText("output.txt")
    src |> parse |> compile |> List.singleton |> showResults |> fprintf f "%s"

g src5
compileSc ("Y", ["f"], ELet (false, ["x", EVar "f"], EAp (EVar "x", EVar "f")))
compileSc ("Y", ["f"], EAp (EVar "f", EAp (EVar "Y", EVar "f")))