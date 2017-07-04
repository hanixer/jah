module Arithmetic

type AExpr =
    | AENum of int
    | AEPlus of AExpr * AExpr
    | AEMult of AExpr * AExpr

let rec aInterpret (expr : AExpr) : int =
    match expr with
    | AENum n -> n
    | AEPlus (e1, e2) -> aInterpret e1 + aInterpret e2
    | AEMult (e1, e2) -> aInterpret e1 * aInterpret e2

type AInstruction =
    | INum of int
    | IPlus
    | IMult

let rec aEval : (AInstruction list * int list) -> int = function
    | [], [n] -> n
    | (INum n) :: instrs, s -> (instrs, (n :: s)) |> aEval
    | IPlus :: instrs, (n0 :: n1 :: s) -> (instrs, (n0 + n1) :: s) |> aEval
    | IMult :: instrs, (n0 :: n1 :: s) -> (instrs, (n0 * n1) :: s) |> aEval
    | _ -> failwith "error in aEval"

let rec aCompile : AExpr -> AInstruction list = function
    | AENum n -> [INum n]
    | AEPlus (e1, e2) ->
        let l = aCompile e1
        let r = aCompile e2
        List.append (List.append l r) [IPlus]
    | AEMult (e1, e2) ->
        let l = aCompile e1
        let r = aCompile e2
        List.append (List.append l r) [IMult]

let prepare is = (is, [])

let run = aCompile >> prepare >> aEval