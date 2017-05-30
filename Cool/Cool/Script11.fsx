#load "Tac.fs"

open Tac

let s = "x <- call in_int
z <- int 0
b <- < x z 
bt b is_negative
output <- x
jmp do_the_printing
label is_negative
output <- - z x"

s |> readTac