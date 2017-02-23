namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module PaketScriptResolver =

    let addPaketReferences (file : LineStr[]) (options : FSharpProjectOptions) =
        printfn "PAKET REFERENCES: %A" options
        options