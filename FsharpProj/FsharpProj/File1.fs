module File1

type Vertex<'TData> = {
    Id : int
    Data : 'TData
    Out : Vertex<'TData> list ref
    In : Vertex<'TData> list ref
}
type Edge<'TData> = Vertex<'TData> * Vertex<'TData>
type Graph<'TData> = (int ref * int ref * (Vertex<'TData> list) ref)

let create () : Graph<'TData> = (ref 0, ref 0, ref [])

let compare v1 v2 = compare v1.Id v2.Id

let isEmpty (c, _) = !c = 0

let vertices (_, v) = !v

let numVertices (c, _) = !c

let edgeInfo e = e

let vertexId v = v.Id

let outgoing src =
    List.map (fun dst -> src, dst) !src.Out

let incomingRev revsrc =
    List.map (fun revdst -> revsrc, revdst) !revsrc.In

let incoming dst =
    List.map (fun src -> src, dst) !dst.In

let edges (_, _, vl) = List.collect outgoing !vl

let addVertex (c, id, vl) =
    incr c
    incr id
    let v = { Id = !id; Data = 1; Out = ref []; In = ref [] }
    vl := v :: !vl
    v

let addEdge (src, dst) =
    src.Out := dst :: !src.Out
    dst.In := src :: !src.In

let removeVertexFromListRef v vl =
    let f v2 =
        vertexId v2 <> vertexId v
    vl := List.filter f !vl

let removeEdge (src, dst) =
    removeVertexFromListRef dst src.Out
    removeVertexFromListRef src src.In

let removeVertex g v =
    List.iter (fun v1 ->
        removeVertexFromListRef v v1.In) !v.Out
    List.iter (fun v2 ->
        removeVertexFromListRef v v2.Out) !v.In

    v.Out := []
    v.In := []

    let (c, _, vl) = g
    decr c
    vl := List.filter (fun v3 -> v3.Id <> v.Id) !vl


/// **Description**
///
/// **Parameters**
///   * `v` - parameter of type `Vertex<'a>`
///
/// **Output Type**
///   * `unit`
///
/// **Exceptions**
///
let printVertex v =
    printfn "vertex id: %d" v.Id
    printf "outgoing: "
    for v in !v.Out do
        printf "%d " v.Id
    printfn ""


/// **Description**
///
/// **Parameters**
///   * `c` - parameter of type `int ref`
///   * `vl` - parameter of type `Vertex<'b> list ref`
///
/// **Output Type**
///   * `unit`
///
/// **Exceptions**
///
let printGraphInfo (c, _, vl) =
    printfn "count: %d" !c
    List.iter printVertex !vl
    printfn ""


(*
let g = create ()

let v1 = addVertex g
let v2 = addVertex g
let v3 = addVertex g
let v4 = addVertex g
let v5 = addVertex g
let v6 = addVertex g
addEdge (v1, v2)
addEdge (v2, v1)
addEdge (v3, v2)
addEdge (v2, v4)
addEdge (v2, v5)
addEdge (v6, v1)
addEdge (v3, v6)
printGraphInfo g
removeVertex g v2
printGraphInfo g
let v4 = addVertex g
printGraphInfo g
*)
let displayGraph g =
    let form = new System.Windows.Forms.Form()
    let viewer = new Microsoft.Msagl.GraphViewerGdi.GViewer()
    let graph = Microsoft.Msagl.Drawing.Graph("graph")

    
    for (v1, v2) in edges g do
        graph.AddEdge(sprintf "%d" v1.Id, sprintf "%d" v2.Id) |> ignore
    
    viewer.Graph <- graph
    form.SuspendLayout()
    viewer.Dock <- System.Windows.Forms.DockStyle.Fill
    form.Controls.Add(viewer)
    form.ResumeLayout()
    form.ShowDialog()
