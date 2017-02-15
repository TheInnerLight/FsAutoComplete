namespace FsAutoComplete

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open System.Collections.Generic

[<AutoOpen>]
module State =
    type VolatileFile =
        { Touched: DateTime
          Lines: string [] }

    type State =
        { Files : Dictionary<SourceFilePath, VolatileFile>
          FileCheckOptions : Dictionary<SourceFilePath, FSharpProjectOptions>
          HelpText : Dictionary<string, FSharpToolTipText>}

        static member Initial =
            { Files = Dictionary()
              FileCheckOptions = Dictionary()
              HelpText = Dictionary() }

        member x.GetCheckerOptions(file: SourceFilePath, lines: LineStr[]) : FSharpProjectOptions =
            let file = Utils.normalizePath file

            let opts =
                match x.FileCheckOptions.TryFind file with
                | None -> State.FileWithoutProjectOptions file
                | Some opts -> opts

            x.Files.[file] <- { Lines = lines; Touched = DateTime.Now }
            x.FileCheckOptions.[file] <- opts
            opts

        member x.AddFileTextAndCheckerOptions(file: SourceFilePath, lines: LineStr[], opts) =
            let file = Utils.normalizePath file
            let fileState = { Lines = lines; Touched = DateTime.Now }
            x.Files.[file] <- fileState
            x.FileCheckOptions.[file] <- opts

        static member private FileWithoutProjectOptions(file) =
            { ProjectFileName = file + ".fsproj"
              ProjectFileNames = [|file|]
              OtherOptions = [|"--noframework"|]
              ReferencedProjects = [| |]
              IsIncompleteTypeCheckEnvironment = true
              UseScriptResolutionRules = false
              LoadTime = DateTime.Now
              UnresolvedReferences = None }

        member x.TryGetFileCheckerOptionsWithLines(file: SourceFilePath) : Result<FSharpProjectOptions * LineStr[]> =
            let file = Utils.normalizePath file
            match x.Files.TryFind(file) with
            | None -> Failure (sprintf "File '%s' not parsed" file)
            | Some (volFile) ->

            match x.FileCheckOptions.TryFind(file) with
            | None -> Success (State.FileWithoutProjectOptions(file), volFile.Lines)
            | Some opts -> Success (opts, volFile.Lines)

        member x.TryGetFileCheckerOptionsWithSource(file: SourceFilePath) : Result<FSharpProjectOptions * string> =
            let file = Utils.normalizePath file
            match x.TryGetFileCheckerOptionsWithLines(file) with
            | Failure x -> Failure x
            | Success (opts, lines) -> Success (opts, String.concat "\n" lines)

        member x.TryGetFileCheckerOptionsWithLinesAndLineStr(file: SourceFilePath, pos : Pos) : Result<FSharpProjectOptions * LineStr[] * LineStr> =
            let file = Utils.normalizePath file
            match x.TryGetFileCheckerOptionsWithLines(file) with
            | Failure x -> Failure x
            | Success (opts, lines) ->
            let ok = pos.Line <= lines.Length && pos.Line >= 1 &&
                     pos.Col <= lines.[pos.Line - 1].Length + 1 && pos.Col >= 1
            if not ok then Failure "Position is out of range"
            else Success (opts, lines, lines.[pos.Line - 1])

module Commands =
    let internal checker = FSharpCompilerServiceChecker()
    let internal state = State.Initial


    let internal tryGetRecentTypeCheckResultsForFile =
        checker.TryGetRecentCheckResultsForFile

    let internal positionHandler f fileName line column  =
        let pos = Pos.make line column
        match state.TryGetFileCheckerOptionsWithLinesAndLineStr(fileName, pos) with
        | Failure s -> async.Return None
        | Success (options, lines, lineStr) ->
        try
            let tyResOpt = tryGetRecentTypeCheckResultsForFile(fileName, options)
            match tyResOpt with
            | None -> async.Return None
            | Some tyRes ->
                async {
                    let! r = Async.Catch (f tyRes pos lineStr lines)
                    match r with
                    | Choice1Of2 r -> return r
                    | Choice2Of2 e -> return None
                }
        with e -> async.Return None

    let internal mapResult f res = async {
        let! result = res
        return
            match result with
            | Result.Failure e -> None
            | Result.Success r -> Some <| f r
    }

    let parse fileName lines version = async {
        let text = String.concat "\n" lines
        let! checkOptions = checker.GetProjectOptionsFromScript(fileName, text)
        state.AddFileTextAndCheckerOptions(fileName, lines, checkOptions)
        let! result = checker.ParseAndCheckFileInProject(fileName, version, text, checkOptions)

        return
            match result with
            | Failure e -> None
            | Success (pr, cr) ->
            match cr with
            | FSharpCheckFileAnswer.Aborted -> None
            | FSharpCheckFileAnswer.Succeeded results ->
                let errors = Array.append results.Errors pr.Errors
                Some <| Response.errors(errors, fileName)
    }

    let declarations file version = async {
        match state.TryGetFileCheckerOptionsWithSource file with
        | Failure s -> return None
        | Success (checkOptions, source) ->
            let! decls = checker.GetDeclarations(file, source, checkOptions, version)
            let decls = decls |> Array.map (fun a -> a,file)
            return Some <| Response.declarations decls
    }

    let helptext sym =
        match state.HelpText.TryFind sym with
        | None -> None
        | Some tip -> Some <| Response.helpText (sym, tip)

    let completion =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _  = async {
            let! res = tyRes.TryGetCompletions pos lineStr (Some "StartsWith")
            return
                match res with
                | Some (decls, residue) ->
                    let declName (d: FSharpDeclarationListItem) = d.Name
                    for decl in decls do
                        state.HelpText.[declName decl] <- decl.DescriptionText
                    Some <| Response.completion decls true
                | None -> None
        }
        positionHandler helper

    let tooltip =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _ =
            tyRes.TryGetToolTip pos lineStr |> mapResult Response.toolTip

        positionHandler helper

    let typesig =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _ =
            tyRes.TryGetToolTip pos lineStr |> mapResult Response.typeSig

        positionHandler helper

    let symbolUse =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _ =
            tyRes.TryGetSymbolUse pos lineStr |> mapResult Response.symbolUse

        positionHandler helper

    let help =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _ =
            tyRes.TryGetF1Help pos lineStr |> mapResult Response.help

        positionHandler helper

    let findDeclarations =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) lineStr _ =
            tyRes.TryFindDeclaration pos lineStr |> mapResult Response.findDeclaration

        positionHandler helper

    let methods =
        let helper (tyRes : ParseAndCheckResults) (pos: Pos) _ (lines: LineStr[]) =
            tyRes.TryGetMethodOverrides lines pos |> mapResult Response.methods

        positionHandler helper





