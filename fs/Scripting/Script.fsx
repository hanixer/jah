#load "Common.fs"
open Common
open System

type Result<'a> =
    | Success of 'a
    | Failure of string list

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