// Define your library scripting code here

let generateUpperLower (s : string) : string list =
    let rec g (cs : char list) : char list list =
        let accumulate elt acc  = 
            match acc with
            | [] -> [ [ System.Char.ToLower(elt) ]; [ System.Char.ToUpper(elt) ] ]
            | _ ->
                let l1 = 
                    (acc |> List.map (fun tails -> System.Char.ToLower(elt) :: tails))
                let l2 =
                    (acc |> List.map (fun tails -> System.Char.ToUpper(elt) :: tails))
                List.append l1 l2

        List.foldBack accumulate cs []

    s
    |> List.ofSeq
    |> g
    |> List.map  (Array.ofList >> System.String.Concat)

generateUpperLower "hello, world" 