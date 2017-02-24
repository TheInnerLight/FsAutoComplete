namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Paket.Domain

module PaketScriptResolver =
    let rec findDependencyFile folder =
        let deps = "paket.dependencies"
        try
            let folder = Path.GetFullPath folder
            let file = folder </> deps
            if File.Exists file then
                Some file
            else
                let dir = Directory.GetParent folder
                findDependencyFile dir.FullName
        with
        | _ -> None

    let getAssemblies dependencyFilePath name =
        let dependenciesFile, lockFile =
            let deps = Paket.Dependencies.Locate dependencyFilePath
            let lock =
                deps.DependenciesFile
                |> Paket.DependenciesFile.ReadFromFile
                |> fun f -> f.FindLockfile().FullName
                |> Paket.LockFile.LoadFrom
            deps, lock

        let frmwrk = Paket.FrameworkIdentifier.DotNetFramework Paket.FrameworkVersion.V4_5
        let deps = lockFile.GetAllDependenciesOf (GroupName "Main", PackageName name)
        deps.Add (PackageName name) |> ignore

        deps
        |> Seq.collect (fun n ->
            let a = dependenciesFile.GetInstalledPackageModel (Some "Main", n.ToString())
            let dlls =
                Paket.LoadingScripts.PackageAndAssemblyResolution.getDllsWithinPackage frmwrk a
                |> List.map (fun f -> f.FullName)
            let asmbls = Paket.LoadingScripts.PackageAndAssemblyResolution.getFrameworkReferencesWithinPackage a
            List.concat [dlls; asmbls]
        )
        |> Seq.toArray

    let addPaketReferences (filePath : string) (file : LineStr[]) (checkOptions : FSharpProjectOptions)  =
        let paketReferences =
            file
            |> Array.where (fun n -> n.StartsWith "#r \"paket:" )
            |> Array.map (fun n -> n.Split([|"nuget"|], StringSplitOptions.RemoveEmptyEntries ).[1].Replace("\"", "").Trim())

        let scriptDirectory = filePath |> Path.GetDirectoryName
        let dependencyFileOpt = scriptDirectory |> findDependencyFile
        let depFile =
            match dependencyFileOpt with
            | Some v -> v
            | None ->
                let p = scriptDirectory </> "paket.dependencies"
                File.AppendAllText (p, "source https://www.nuget.org/api/v2/")
                p

        let assemblies =
            paketReferences
            |> Array.collect(getAssemblies depFile)

        // printfn "PAKET REFERENCES: %A" paketReferences
        // printfn "PAKET.DEPENDENCIES: %A" depFile
        // printfn "RESOLVED ASSEMBLIES: %A" assemblies

        let frmwkLocation = @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\"

        let assembliesRefs =
            assemblies
            |> Array.map (fun n -> if Path.IsPathRooted n then n else frmwkLocation </> n + ".dll")
            |> Array.map ((fun n -> n.Replace("\\", "\\\\")) >> (sprintf "-r:%s"))

        { checkOptions with OtherOptions = [| yield! checkOptions.OtherOptions; yield! assembliesRefs |] }