// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open File1

[<EntryPoint>]
let main argv = 
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
    addEdge (v5, v3)
    addEdge (v5, v1)

    displayGraph g
    0 // return an integer exit code
