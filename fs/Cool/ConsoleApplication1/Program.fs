// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open CoolAst
open CoolType

[<EntryPoint>]
let main argv = 
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
            [Method ((13, "f"),[],(13, "Int"),(13, Integer 1));
            Method ((14, "da"),[],(14, "Int"),(14, Integer 3))])]

    let x = 
        inheritanceMap astBadTypes
        |> mapRes (Map.ofList >> 
            getInheritedMethodsErrors "C" astBadTypes)

    0 // return an integer exit code
