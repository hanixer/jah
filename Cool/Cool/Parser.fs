module Parser

open CoolAst
open FParsec


let pCoolString : Parser<string, unit> = regex "\"[^\n]*\""