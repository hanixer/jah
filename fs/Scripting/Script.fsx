#load "Common.fs"
open Common
open System

type Result<'a> =
    | Success of 'a
    | Failure of string list



module Result = 

    let map f xResult = 
        match xResult with
        | Success x ->
            Success (f x)
        | Failure errs ->
            Failure errs
    // Signature: ('a -> 'b) -> Result<'a> -> Result<'b>

    // "return" is a keyword in F#, so abbreviate it
    let retn x = 
        Success x
    // Signature: 'a -> Result<'a>

    let apply fResult xResult = 
        match fResult,xResult with
        | Success f, Success x ->
            Success (f x)
        | Failure errs, Success x ->
            Failure errs
        | Success f, Failure errs ->
            Failure errs
        | Failure errs1, Failure errs2 ->
            // concat both lists of errors
            Failure (List.concat [errs1; errs2])
    // Signature: Result<('a -> 'b)> -> Result<'a> -> Result<'b>

    let bind f xResult = 
        match xResult with
        | Success x ->
            f x
        | Failure errs ->
            Failure errs

type CustId = CustId of string
type ProductId = ProductId of string
type ProductInfo = {ProductName: string; } 

// Define your library scripting code here
type ApiClient() =
    // static storage
    static let mutable data = Map.empty<string,obj>

    /// Try casting a value
    /// Return Success of the value or Failure on failure
    member private this.TryCast<'a> key (value:obj) =
        match value with
        | :? 'a as a ->
            Result.Success a 
        | _  ->                 
            let typeName = typeof<'a>.Name
            Result.Failure [sprintf "Can't cast value at %s to %s" key typeName]

    /// Get a value
    member this.Get<'a> (id:obj) = 
        let key =  sprintf "%A" id
        printfn "[API] Get %s" key
        printfn "%A" data
        match Map.tryFind key data with
        | Some o -> 
            this.TryCast<'a> key o
        | None -> 
            Result.Failure [sprintf "Key %s not found" key]

    /// Set a value
    member this.Set (id:obj) (value:obj) = 
        let key =  sprintf "%A" id
        printfn "[API] Set %s" key
        if key = "bad" then  // for testing failure paths
            Result.Failure [sprintf "Bad Key %s " key]
        else
            data <- Map.add key value data 
            Result.Success ()
           
    member this.Open() =
        printfn "[API] Opening"

    member this.Close() =
        printfn "[API] Closing"

    interface System.IDisposable with
        member this.Dispose() =
            printfn "[API] Disposing"

            
let getPurchaseInfo (custId:CustId) : Result<ProductInfo list> = 
    let retn = Success
    let (>>=) xRes f  = 
        match xRes with
        | Success x -> f x
        | Failure errs -> Failure errs
    use api = new ApiClient()
    let rec prodIdListToResult (prodIds:ProductId list) =

        match prodIds with
        | [] -> retn []
        | (p::ps) -> (api.Get<ProductInfo> p) >>= (fun h ->
            prodIdListToResult ps >>= (fun t ->
            retn (h::t)))

    api.Open()

    let productIds = api.Get<ProductId list> custId
    let finResult = productIds >>= prodIdListToResult
    api.Close()

    finResult

let api = new ApiClient()
api.Set (CustId "a") (List.map (sprintf "%d" >> ProductId) [2;1;3;4]) |> ignore
api.Set (ProductId "2") ({ProductName="mizro"})
api.Set (ProductId "3") ({ProductName="funk"})
api.Set (ProductId "4") ({ProductName="bank"})
getPurchaseInfo (CustId "a")

let executeApiAction apiAction =
    use api = new ApiClient()

    api.Open()

    let result = apiAction api

    api.Close()

    result

type ApiAction<'a> = ApiAction of (ApiClient -> 'a)

let getPurchaseIds (custId:CustId)  =
    let f (api:ApiClient) =
        api.Get<ProductId list> custId
    ApiAction f

let getProductInfo (productId:ProductId)=
    let f  (api:ApiClient) =
        api.Get<ProductInfo> productId
    ApiAction f

module ApiAction =
    let run api (ApiAction action) =
        action api

    let map f action =
        let newAction api =
            let x = run api action
            f x
        ApiAction newAction

    let retn x =
        let newAction api =
            x
        ApiAction newAction

    let apply fAction xAction =
        let newAction api =
            let f = run api fAction
            let x = run api xAction
            f x
        ApiAction newAction

    let bind f xAction = 
        let newAction api =
            let x = run api xAction
            run api (f x)
        ApiAction newAction

    let execute action =
        use api = new ApiClient()
        api.Open()
        let result = run api action
        api.Close()
        result

module ApiActionResult = 
    let map f =
        ApiAction.map (Result.map f)

    let retn x =
        ApiAction.retn (Result.retn x)

    let apply fActionResult xActionResult =
        let newAction api =
            let fResult = ApiAction.run api fActionResult 
            let xResult = ApiAction.run api xActionResult
            Result.apply fResult xResult
        ApiAction newAction

    let bind f xActionResult =
        let newAction api =
            let xResult = ApiAction.run api xActionResult
            match xResult with
            | Success x ->
                let yActionResult = f x
                ApiAction.run api yActionResult
            | Failure errs -> 
                Failure errs
        ApiAction newAction

    let either onSuccess onFailure xActionResult =
        let newAction api =
            let xResult = ApiAction.run api xActionResult
            let yAction = 
                match xResult with
                | Success x -> onSuccess x
                | Failure errs -> onFailure errs
            ApiAction.run api yAction
        ApiAction newAction

let traverseAction f list =
    let (<*>) = ApiActionResult.apply
    let retn = ApiActionResult.retn
    let cons head tail = head :: tail
    let initState = retn []
    let folder head tail =
        retn cons <*> f head <*> tail

    List.foldBack folder list initState
    
let traverseLogging logi f list =
    let (<*>) = ApiActionResult.apply
    let retn = ApiActionResult.retn
    let cons head tail = head :: tail
    let initState = retn []
    let folder head tail =
        (f head)
        |> ApiActionResult.either
            (fun h -> retn cons <*> retn h <*> tail)
            (fun errs -> logi errs; tail)

    List.foldBack folder list initState

let getPurchaseInfo' =
    let getProductInfoLifted = 
        getProductInfo
        |> traverseAction
        |> ApiActionResult.bind
    getPurchaseIds >> getProductInfoLifted

let showResult result =
    match result with
    | Success (productInfoList) -> 
        printfn "SUCCESS: %A" productInfoList
    | Failure errs -> 
        printfn "FAILURE: %A" errs

let setupTestData (api:ApiClient) =
    //setup purchases
    api.Set (CustId "C1") [ProductId "P1"; ProductId "P2"] |> ignore
    api.Set (CustId "C2") [ProductId "PX"; ProductId "P2"] |> ignore

    //setup product info
    api.Set (ProductId "P1") {ProductName="P1-Name"} |> ignore
    api.Set (ProductId "P2") {ProductName="P2-Name"} |> ignore
    // P3 missing

// setupTestData is an api-consuming function
// so it can be put in an ApiAction 
// and then that apiAction can be executed
let setupAction = ApiAction setupTestData
ApiAction.execute setupAction 

CustId "C2"
|> getPurchaseInfo'
|> ApiAction.execute
|> showResult

let getPurchaseInfoWithLog =
    let log errs = printfn "SKIPPED %A" errs
    let getProductInfoLifter =
        getProductInfo
        |> traverseLogging log
        |> ApiActionResult.bind
    getPurchaseIds >> getProductInfoLifter

    
CustId "C2"
|> getPurchaseInfoWithLog
|> ApiAction.execute
|> showResult

