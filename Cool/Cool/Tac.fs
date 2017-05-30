module Tac

open System.Text.RegularExpressions

type Rhs =
    | Var of string
    | BinOp of string * string * string
    | Not of string
    | Negate of string
    | New of string
    | Default of string
    | Isvoid of string
    | FunCall of string * (string list)
    | IntRhs of int
    | BoolRhs of bool
    | StringRhs of string

type TacEntry =
    | Assignment of string * Rhs
    | Jump of string 
    | Label of string
    | Return of string
    | Comment
    | BranchTrue of string * string

let readTac (s : string) : TacEntry list =
    let mergeStringLiterals lines =
        lines
        |> List.fold (fun (acc : string list) (line : string) ->
            match acc with
            | prev :: rest when Regex.IsMatch(prev, ".*<-.*string") ->
                (prev + (" ") + line) :: rest
            | _ -> line :: acc) []

    let processLine (line : string) =
        let words = line.Split(' ') |> List.ofArray
        match words with
        | ["jmp"; label] -> Jump label
        | ["label"; label] -> Label label
        | ["return"; var] -> Return var
        | ["bt"; var; label] -> BranchTrue (var, label)
        | [var; "<-"; "call"; f; arg] -> Assignment (var, FunCall (f, [arg]))
        | [var; "<-"; op; v1; v2] -> Assignment (var, BinOp (op, v1, v2))
        | [var; "<-"; "int"; n] -> Assignment (var, IntRhs (int n))
        | [var; "<-"; "bool"; n] -> Assignment (var, BoolRhs (System.Boolean.Parse n))
        | [var; "<-"; "string"; s] -> Assignment (var, StringRhs s)
        | [var; "<-"; "call"; f] -> Assignment (var, FunCall (f, []))
        | [var; "<-"; "not"; x] -> Assignment (var, Not x)
        | [var; "<-"; "~"; x] -> Assignment (var, Negate x)
        | [var; "<-"; "new"; x] -> Assignment (var, New x)
        | [var; "<-"; "default"; x] -> Assignment (var, Default x)
        | [var; "<-"; "isvoid"; x] -> Assignment (var, Isvoid x)
        | [var; "<-"; varRhs] -> Assignment (var, Var varRhs)
        | head :: _ when head.StartsWith("#") || head.StartsWith(";") -> 
            Comment
        | _ -> Comment
        
    s.Split('\n')
    |> List.ofArray
    |> mergeStringLiterals
    |> List.map processLine