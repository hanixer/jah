#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
// #r @"bin\Debug\Cool.dll"
#load "Graphs.fs"
#load "CoolAst.fs"
#load "CoolType.fs"

open System.IO
open CoolAst
open CoolType
open Graphs

// let aste : Ast<int> = 
//     Ast [
//         Class ((1,"A"),None,[]);
//         Class ((2, "B"),Some (1, "A"),[]);
//         Class ((3, "C"),Some (2, "B"),[])]
// let ast2 : Ast<int> = 
//     Ast [
//         Class ((1,"A"),None,[]);
//         Class ((2, "C"),Some (1, "A"),[]);
//         Class ((3, "C"),Some (2, "B"),[])]
// let ast3 : Ast<int> = 
//     Ast [
//         Class ((1,"A"),Some (1, "D"),[]);
//         Class ((2, "C"),Some (1, "A"),[]);
//         Class ((2, "D"),Some (1, "C"),[]);
//         Class ((2, "E"),Some (1, "D"),[]);
//         Class ((3, "gap"),Some (3, "c1"),[]);
//         Class ((3, "c1"),Some (3, "gap"),[]);
//         Class ((3, "poairpoie"),None,[])]

// let y = inheritanceMap aste
// inheritanceMap ast2
// inheritanceMap ast3

// [1..10] |> List.choose (fun x -> if x % 2 = 0 then Some (x + 1) else None)
(*
let ast = 
 Ast
    [Class
    ((1, "Main"),Some (1, "IO"),
        [Method
        ((2, "main"),[],(2, "Object"),
            (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
    Class
    ((6, "A"),None,
        [Method ((7, "f"),[],(7, "Int"),(7, Integer 1));
        Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
        Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
    Class
    ((12, "C"),None,
        [Method ((13, "f"),[],(13, "Int"),(13, Integer 1));
        Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]
let badast = 
 Ast
    [Class
    ((1, "Main"),Some (1, "IO"),
        [Method
        ((2, "main"),[],(2, "Object"),
            (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
    Class
    ((6, "A"),None,
        [Method ((7, "f"),[],(7, "Int"),(7, Integer 1));
        Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
        Method ((60, "f"),[],(11, "Int"),(7, Integer 1));
        Method ((77, "f"),[],(11, "Int"),(7, Integer 1));
        Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
    Class
    ((12, "C"),None,
        [Method ((13, "f"),[],(13, "Int"),(13, Integer 1));
        Method ((55, "f"),[],(44, "Int"),(13, Integer 1));
        Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]

validateMethodRedefinition badast
let badast2 = 
 Ast
    [Class
    ((1, "Main"),Some (1, "IO"),
        [Method
        ((2, "main"),[],(2, "Object"),
            (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
    Class
    ((6, "A"),None,
        [Method ((7, "f"),[(1,"x"),(1,"Int");(1,"x"),(2,"Bool")],(7, "Int"),(7, Integer 1));
        Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
        Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
    Class
    ((12, "C"),None,
        [Method ((13, "f"),[(1,"x"),(1,"Int");(1,"x"),(2,"Bool")],(13, "Int"),(13, Integer 1));
        Method ((14, "da"),[(1,"y"),(1,"Int");(1,"y"),(2,"Bool");(1,"y"),(2,"Object");(1,"x"),(1,"Int");(1,"x"),(2,"Bool")],(14, "Int"),(14, Integer 3))])]

validateMethodFormalsRedefinition badast2
inheritanceMap badast2
ast2inheritanceGraph ast |> mapRes Graphs.toposort
getClassMethods "IO" ast
inheritanceMap ast |> mapRes (Map.ofList >> getInheritedMethods "Main" ast)
validateRedefinedMethods ast
let astBadTypes = 
    Ast
        [Class
        ((1, "Main"),Some (1, "IO"),
            [Method
            ((2, "main"),[],(2, "String"),
                (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
        Class
        ((6, "A"),None,
            [Method ((7, "f"),[],(7, "Int"),(7, Integer 1));
            Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
            Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
        Class
        ((12, "C"), Some (0, "A"),
            [Method ((13, "f"),[],(13, "String"),(13, Integer 1));
            Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]
let astBadTypes2 = 
    Ast
        [Class
        ((1, "Main"),Some (1, "IO"),
            [Method
            ((2, "main"),[],(2, "String"),
                (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
        Class
        ((6, "A"),None,
            [Method ((7, "f"),[(0,"x"),(0,"Int")],(7, "Int"),(7, Integer 1));
            Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
            Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
        Class
        ((12, "C"), Some (0, "A"),
            [Method ((13, "f"),[(13,"x"),(15,"Der")],(13, "Int"),(13, Integer 1));
            Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]

let ast2 = 
 Ast
    [Class
    ((1, "Main"),Some (1, "IO"),
        [Method
        ((2, "main"),[],(2, "Object"),
            (3, SelfDispatch ((3, "out_string"),[(3, String "Hello, world.\n")])))]);
    Class
    ((6, "A"),None,
        [Method ((7, "f"),[],(7, "Int"),(7, Integer 1));
        Attribute ((0, "x"),(0,"Int"), None);
        Method ((8, "g"),[],(8, "Int"),(8, Integer 2));
        Method ((10, "as"),[],(10, "Int"),(10, Integer 4))]);
    Class
    ((12, "C"),Some (0,"A"),
        [Method ((13, "f"),[],(13, "Int"),(13, Integer 1));
        Attribute ((0, "y"),(0,"Int"), None);
        Attribute ((10, "y"),(0,"Int"), None);
        Attribute ((2, "x1"),(3,"Int"), None);
        Attribute ((5, "x2"),(3,"Int"), None);
        Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]

validateRedefinedMethods astBadTypes2
ast2methodEnvironment ast |> getMethodType "w" "f"
getInheritedAttributes "C" ast2 (inheritanceMapUnchecked ast2)
validateRedefinedAttributes ast2
*)
typecheck2 1 2 ({ Type = None; Loc = 2; Expr = {Type = None; Loc = 5; Expr = True } |> Negate } )
typecheck2 1 2 { 
        Type = None; Loc = 2; 
        Expr = 
            Plus (
                { Type = None; Loc = 2; Expr = Integer 5 },
                { Type = None; Loc = 3; Expr = True } ) }
                 
