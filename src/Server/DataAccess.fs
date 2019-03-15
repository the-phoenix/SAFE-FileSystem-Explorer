module DataAccess

open System.Threading.Tasks
open System.IO
open FSharp.Control.Tasks.V2

open Shared
open System

let getInitCounter() : Task<Counter> = task { return { Value = 42 } }

let (|EndWithExtension|_|) (fileExt: string) (filePath: string) =
    let extracted = filePath |> Path.GetExtension

    if System.String.Equals(fileExt, filePath, System.StringComparison.InvariantCultureIgnoreCase) then
        Some()
    else
        None

let (|IsTextFile|_|) filePath =
    match filePath with
    | EndWithExtension "txt"
    | EndWithExtension "md"
    | EndWithExtension "rtf"
        -> Some()
    | _ -> None

let private getFSItemInformationsInsideDirectory path =
    path
    |> Directory.GetFileSystemEntries
    |> Seq.sort
    |> Seq.toArray
    |> Array.map(fun path ->
        let attr = File.GetAttributes path

        if attr.HasFlag(FileAttributes.Directory) then
            Directory { Name = path |> Path.GetFileName; FullPath = path }
        else
            File {
                Name = path |> Path.GetFileName
                ContentReadable = match path with | IsTextFile -> true | _ -> false
                FullPath = path
            }
    )

let getEntriesInDirectory path : Task<DirectoryListResponse> = task {
    let contents = if Directory.Exists path then DirResult ( getFSItemInformationsInsideDirectory path ) else InvalidPath

    return contents
}

let a = System.IO.Path.Combine("one", "two")