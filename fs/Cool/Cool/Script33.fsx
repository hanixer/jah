#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsecCS.dll"
#r @"..\..\packages\FParsec\lib\portable-net45+netcore45+wpa81+wp8\FParsec.dll"  
#r @"bin\Debug\Cool.dll"
#load "CoolType.fs"

open System.IO
open CoolAst
open CoolType

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

validateMethodRedefinition ast