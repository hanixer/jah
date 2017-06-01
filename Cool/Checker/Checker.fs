module Checker

open System.IO
open CoolAst
open CoolType

type ProcessResult = { exitCode : int; stdout : string; stderr : string }

let ps f = fprintfn f "%s"

let executeProcess (exe,cmdline) =
    let psi = System.Diagnostics.ProcessStartInfo(exe,cmdline) 
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true        
    let p = System.Diagnostics.Process.Start(psi) 
    let output = System.Text.StringBuilder()
    let error = System.Text.StringBuilder()
    p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    p.BeginErrorReadLine()
    p.BeginOutputReadLine()
    p.WaitForExit()
    { exitCode = p.ExitCode; stdout = output.ToString(); stderr = error.ToString() }

let getAst (srce) =
    if File.Exists(srce) then
        Parser.parse (File.ReadAllText(srce))
    else        
        Failure [sprintf "File %s does not exist" srce]

let printId f (loc, n) = fprintf f "%d\n%s\n" loc n

let printList f printer xs =
    List.length xs |> fprintfn f "%d"
    for x in xs do
        printer f x

let rec printExpr f (expr:Expr) =
    let p = fprintfn f
    let ps = fprintfn f "%s"
    p "%d" expr.Loc

    match expr.Type with
    | Some (Type t) ->
        ps t
    | Some (SelfType _) | Some SelfTypeFree ->
        ps "SELF_TYPE"
    | _ -> ignore 1

    match expr.Expr with
    | Assign (n, e) -> 
        ps "assign"
        printId f n
        printExpr f e
    | DynDispatch (a,b,c) ->
        ps "dynamic_dispatch"
        printExpr f a
        printId f b
        printList f printExpr c
    | StatDispatch (a,b,c,d) ->
        ps "static_dispatch"
        printExpr f a
        printId f b
        printId f c
        printList f printExpr d
    | SelfDispatch (a, b) ->
        ps "self_dispatch"
        printId f a
        printList f printExpr b
    | If (a,b,c) ->
        ps "if"
        printExpr f a
        printExpr f b
        printExpr f c
    | While (a,b) ->
        ps "while"
        printExpr f a
        printExpr f b
    | Block xs ->
        ps "block"        
        printList f printExpr xs
    | Let (bindings, e) ->
        let printBinding f (a,b,c) =
            match c with
            | Some cc ->
                ps "let_binding_init"
                printId f a
                printId f b
                printExpr f cc
            | None ->
                ps "let_binding_no_init"
                printId f a
                printId f b
        ps "let"
        printList f printBinding bindings
        printExpr f e
    | New n ->
        ps "new"
        printId f n
    | Isvoid e ->
        ps "isvoid"
        printExpr f e
    | Plus (e1, e2) ->
        ps "plus"
        printExpr f e1    
        printExpr f e2
    | Minus (e1, e2) ->
        ps "minus"
        printExpr f e1    
        printExpr f e2
    | Times (e1, e2) ->
        ps "times"
        printExpr f e1    
        printExpr f e2
    | Divide (e1, e2) ->
        ps "divide"
        printExpr f e1    
        printExpr f e2
    | LT (e1, e2) ->
        ps "lt"
        printExpr f e1    
        printExpr f e2
    | LE (e1, e2) ->
        ps "le"
        printExpr f e1    
        printExpr f e2
    | EQ (e1, e2) ->
        ps "eq"
        printExpr f e1    
        printExpr f e2
    | Not e ->
        ps "not"
        printExpr f e
    | Negate e ->
        ps "negate"
        printExpr f e
    | Identifier n ->
        ps "identifier"
        printId f n    
    | True -> ps "true"
    | False -> ps "false"
    | Integer n -> 
        ps "integer"
        p "%d" n
    | String s -> 
        ps "string"
        ps s
    | Case (a,b) ->
        let printCase f (x,y,z) =
            printId f x
            printId f y
            printExpr f z
        ps "case"
        printExpr f a
        printList f printCase b
    ()

let printFormal f ((a,b):Formal) =
    printId f a
    printId f b

let printFeature f (feat:Feature) =
    let p = fprintfn f
    let ps = fprintfn f "%s"
    match feat with
    | Method (a,b,c,d) ->
        ps "method"
        printId f a
        printList f printFormal b
        printId f c
        printExpr f d
    | Attribute (a, b, Some c) ->
        ps "attribute_init"
        printId f a
        printId f b
        printExpr f c
    | Attribute (a, b, None) ->
        ps "attribute_no_init"
        printId f a
        printId f b

let printClass f (Class (a,b,c)) = 
    let p = fprintfn f
    let ps = fprintfn f "%s"
    printId f a
    match b with
    | Some p ->
        ps "inherits"
        printId f p
    | None ->
        ps "no_inherits"
    printList f printFeature c

let printAttribute f ((_,a), b, c) =
    let ps = fprintfn f "%s"
    match c with
    | Some cc ->
        ps "initializer"
        ps a
        ps (snd b)
        printExpr f cc
    | None ->
        ps "no_initializer"
        ps a
        ps (snd b)

let printClassesAndAttributes f attrs =
    let ps = fprintfn f "%s"
    fprintfn f "%s" "class_map"
    fprintfn f "%d" (Seq.length attrs)
    attrs
    |> Map.iter (fun k v ->
        ps k
        printList f printAttribute v)

// Id * string list * (*ultimate parent name*)string * MethodBody
let printMethod f (a, b, c, d) =
    ps f (snd a)
    printList f ps b
    ps f c
    match d with
    | BodyInner (_, typ, cl) ->
        fprintfn f "0"
        ps f typ
        ps f "internal"
        ps f (cl + "." + (snd a))
    | BodyExpr expr ->
        printExpr f expr


let printImplementationMap f implMap =
    ps f "implementation_map"
    fprintfn f "%d" (Seq.length implMap)
    implMap
    |> Map.iter (fun k v ->
        ps f k
        printList f printMethod v)

let printParentMap f parMap =
    ps f "parent_map"
    parMap
    |> Map.toList
    |> printList f (fun f (c, p) ->
        ps f c
        ps f p) 

let printSemanticInfo f (semInfo:SemanticInfo) =    
    printClassesAndAttributes f semInfo.Attributes
    printImplementationMap f semInfo.Methods
    printParentMap f semInfo.InheritanceMap    
    match semInfo.AnnotatedAst with
    | Ast cs -> printList f printClass cs

[<EntryPoint>]
let main argv =
    let outFilename = "out.cl-ast"
    use f = File.CreateText("testing.txt")
    printExpr f {Loc = 5; Type = None; Expr = True}
    match argv with
    | [|arg|] ->
        match getAst arg with
        | Success ast ->
            match analyze ast with
            | Success semInfo ->
                use fout = File.CreateText(outFilename)
                printSemanticInfo fout semInfo
                0
            | Failure errs ->
                printfn "Semantic errors happend!"
                printfn "%A" errs
                -1
        | Failure errs ->
            printfn "Parse error.\n%A" errs
            1
    | _ -> 
        printfn "Wrong invocation. Expected exactly one argument"
        -1