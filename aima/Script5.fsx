open System.IO
open System

type Itemset = int list
type Sequence = Itemset list



let readValues file addItem emptyItemset =
    let folder = (fun (acc, res) x ->
        if x = -1 then (emptyItemset, acc::res)
        else if x = -2 then (acc, res)
        else (addItem x acc, res) )

    let mapper = (List.fold folder (emptyItemset,[])) >> snd

    [for line in File.ReadAllLines(file) do
        yield 
         [for pieces in line.Split(' ') do
            yield Int32.TryParse(pieces) |> snd]]
    |> List.map mapper

let readValuesSet file = readValues file Set.add Set.empty
let readValuesList file = readValues file (fun x xs -> x::xs) []

let isSubSequence s1 s2 =
    let rec find a s2 =
        match s2 with
        | b::restS2 ->
            if Set.isSubset a b then
                Some restS2
            else
                find a restS2
        | [] -> 
            None

    let rec go s1 s2 =
        match s1 with
        | a::restS1 ->
            match find a s2 with
            | Some restS2 ->
                go restS1 restS2
            | None ->
                false
        | [] -> true

    go s1 s2

let projection alpha beta =
    let rec find b = function
        | a::restAlpha ->
            if Set.isSubset b a then
                Some (a, restAlpha)
            else find b restAlpha
        | [] -> None
 
    let rec go alpha beta res =
        match beta with
        | [] -> []
        | [b] ->
            match find b alpha with
            | Some (a, restAlpha) ->
                List.append (List.rev res) (a::restAlpha)
            | None ->
                []
        | b::restBeta ->
            match find b alpha with
            | Some (_, restAlpha) ->
                go restAlpha restBeta (b::res)
            | None ->
                []
    go alpha beta []


let rec matchElems b a  =
    let rec go b a =
        printfn "%A ?? %A" b a
        match b, a with
        | [], _ -> None
        | _, [] -> None
        | b1::restB, a1::restA ->
            if b1 = a1 then
                if restB.IsEmpty then
                    Some restA
                else
                    go restB restA
            else
                None

    go b a

let rec countItemsInSequence s =
    List.fold (fun count elem ->
        List.fold (fun count item ->
            if Map.containsKey item count then
                count
            else
                Map.add item 1 count) 
         count elem) 
     Map.empty s

let countItemsInDatabase d =
    let mergeCounts c1 c2 =
        Map.fold (fun acc k v ->
            match Map.tryFind k acc with
            | Some v2 -> Map.add k (v + v2) acc
            | None -> Map.add k v acc) c1 c2

    List.fold (fun count s ->
        countItemsInSequence s
        |> mergeCounts count) Map.empty d

let generateItemSupport database prefix ignoreFirst =
    let isPrefixMatch elem =
        List.forall (fun item -> Seq.contains item elem) prefix

    let isItemInPrefix item = List.contains item prefix 

    let rec supportOfElem support elem  =
        List.fold (fun support item ->
            if isItemInPrefix item || Map.containsKey item support then
                support
            else
                Map.add item 1 support) support elem

    let generateForSequence sequence =
        let sequence = 
            if ignoreFirst && List.isEmpty sequence |> not then
                List.tail sequence
            else
                sequence

        List.fold (fun support elem ->
            if isPrefixMatch elem then
                supportOfElem support elem
            else support) Map.empty sequence

    let mergeSupports support1 support2 =
        Map.fold (fun support k v ->
            match Map.tryFind k support with
            | Some v2 -> Map.add k (v + v2) support
            | None -> Map.add k v support) support1 support2
            
    List.fold (fun support sequence ->
        generateForSequence sequence
        |> mergeSupports support) Map.empty database
    |> Map.toList
    


let rec projectSequence sequence prefix ignoreFirst =
    let rec isPrefixMatches elem =
        List.forall (fun item -> List.contains item elem) prefix
        
    match sequence with
    | [] -> None
    | elem1::elems ->
        if ignoreFirst then
            projectSequence elems prefix false
        else if isPrefixMatches elem1 then
            Some sequence
        else
            projectSequence elems prefix false

let projectDatabase database prefix ignoreFirst = [
    for sequence in database do
        let s = projectSequence sequence prefix ignoreFirst
        if s.IsSome then yield s.Value
]

let rec prefixSpanImpl database minsup prefix prevPattern = 
    let spanForPrefix prefix pattern ignoreFirst = [
        let support = 
            generateItemSupport database prefix ignoreFirst
        
        printfn "   supp: %A" support

        for (item, n) in support do
            if n >= minsup then
                let newPrefix = List.append prefix [item]
                let newDatabase = projectDatabase database newPrefix ignoreFirst
                yield! prefixSpanImpl newDatabase minsup newPrefix pattern
    ]

    [
        let newPattern = 
            if not (List.isEmpty prefix) then
                List.append prevPattern [prefix]
            else
                prevPattern

        yield (newPattern, Seq.length database)
        printfn "database: %A" database
        printfn "prefix: %A" prefix
        printfn "newPattern: %A" newPattern

        
        yield! spanForPrefix  [] newPattern true
 
        yield! spanForPrefix  prefix newPattern false
    ]


let prefixSpan (database : 'a list list list) minsup = [
    prefixSpanImpl database minsup [] []
]

let alpha = [
        List.singleton 1
        [1; 2; 3]
        [1; 2; 3]
        List.singleton 4
        [3; 6]
    ]
let somewat = [
    List.singleton 4
    List.singleton 6
    [3; 6]
]

let beta = [
    [2; 3]
    List.singleton 2
]

let database = [
    [[1]; [1;2;3]; [1;3]; [4]; [3; 6]]
    [[1;4]; [3]; [2;3]; [1;5]]
    [[5;6]; [1;2]; [4;6]; [3]; [2]]
    [[5;]; [7]; [1;6];[3];[2];[3]]
]

readValues "C:\\Users\\Compi\\Desktop\\ca\\tf\\contextCloSpan.txt"

countItemsInDatabase database
matchElems [2;3] [1;2;3]
let s = [["a"]; ["b"; "c"]; ["a"; "c"]; ["c"]]
let p = ["b"]

let datasetProject = [
            [["a"]; ["a"; "b"; "c"]; ["a"; "c"]; ["d"]; ["c"; "f"]];
            [["a"; "d"]; ["c"]; ["b"; "c"]; ["a"; "e"]];
            [["e"; "f"]; ["a"; "b"]; ["d"; "f"]; ["d"]; ["b"]];
            [["e"]; ["g"]; ["a"; "f"]; ["c"]; ["b"]; ["c"]]
        ]

generateItemSupport datasetProject []
projectDatabase datasetProject ["a"] true
prefixSpan datasetProject 2
