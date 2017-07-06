// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"
#r "packages/Fantomas/lib/FantomasLib.dll"

open Fake
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"
let fantomasConfig =
    { FormatConfig.Default with
            PageWidth = 120
            ReorderOpenDeclaration = true }


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

let templateOnly =
    !! "Template/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTemplate" (fun _ ->
    MSBuildDebug buildDir "Build" templateOnly
    |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "FormatCode" (fun _ ->
    !! "src/**/*.fs"
      |> formatCode fantomasConfig
      |> Log "Formatted files: "
)

// Build order
"Clean"
  ==> "Build"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"
