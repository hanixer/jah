
#r "..\\packages\\Microsoft.Msagl.1.1.1\\lib\\net40\\Microsoft.Msagl.dll"
#r "..\\packages\\Microsoft.Msagl.Drawing.1.1.1\\lib\\net40\\Microsoft.Msagl.Drawing.dll"
#r "..\\packages\\Microsoft.Msagl.GraphViewerGDI.1.1.1\\lib\\net40\\Microsoft.Msagl.GraphViewerGdi.dll"
#load "File1.fs"

open File1

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
