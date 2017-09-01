// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open ClangSharp

[<EntryPoint>]
let main argv = 
    let visitit (child, parent) =
        Cursor.ChildVisitResult.Break

    use idx = new ClangSharp.Index()
    let unit = idx.CreateTranslationUnit(@"D:\tmp\some.cpp")
    
    printfn "%A" argv
    0 // return an integer exit code
