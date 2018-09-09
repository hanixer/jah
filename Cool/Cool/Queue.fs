module Queue
    let empty = ([], [])
    let isEmpty = function
        | [], [] -> true
        | _ -> false

    let enqueue x (ins, outs) = x :: ins, outs
    let enqueueList xs (ins, outs) = List.append xs ins, outs
    let exists f (ins, outs) = 
        if List.exists f ins then true
        else if outs = [] then false
        else List.exists f outs
    let enqueueListUnique isEqual xs q =         
        List.foldBack (fun x q ->
            printfn "lisuniq: x<%A> q<%A>" x q
            if exists (isEqual x) q then q
            else enqueue x q) xs q

    let dequeue (ins, outs) =
        match ins, outs with
        | [], [] -> failwith "dequeue: Queue is empty"
        | _, x :: outs -> x, (ins, outs)
        | _, _ ->
            let outs = List.rev ins
            List.head outs, ([], List.tail outs)
    let tryDequeue (ins, outs as q) =
        if isEmpty q then None
        else dequeue q |> Some
    let singleton x = enqueue x empty
        
