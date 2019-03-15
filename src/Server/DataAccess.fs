module DataAccess

open System.Threading.Tasks
open System.IO
open FSharp.Control.Tasks.V2

open Shared

let getInitCounter() : Task<Counter> = task { return { Value = 42 } }

let (|EndWithExtension|_|) (fileExt: string) (filePath: string) =
    let extracted = filePath |> Path.GetExtension

    if System.String.Equals(fileExt, filePath, System.StringComparison.InvariantCultureIgnoreCase) then
        Some()
    else
        None

let (|IsSmallFile|_|) filePath =
    if System.IO.FileInfo(filePath).Length < 10240L then // under 10 KBytes
        Some()
    else
        None

let (|IsReadableFile|_|) filePath =
    match filePath with
    // | EndWithExtension "txt" | EndWithExtension "md" | EndWithExtension "rtf" | EndWithExtension "rtf"
    //    -> match filePath with
        | IsSmallFile -> Some() | _ -> None
    // | _ -> None

let ArrayConcat arr1 arr2 =
    Array.concat [arr1; arr2]

let private getFSItemInformationsInsideDirectory path =
    let preFSEntries = [|
        Directory { Name = "."; FullPath = path };
        Directory { Name = ".."; FullPath = Directory.GetParent(path).FullName };
    |]

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
                    ContentReadable = match path with | IsReadableFile -> true | _ -> false
                    FullPath = path
                }
        )
        |> ArrayConcat preFSEntries


let getEntriesInDirectory path : Task<DirectoryListResponse> = task {
    let contents = if Directory.Exists path then DirResult ( getFSItemInformationsInsideDirectory path ) else InvalidPath

    return contents
}

let getFileContent filePath : Task<FileContentResponse> = task {
    let content = if File.Exists filePath then FileResult (File.ReadAllText filePath) else NotExists

    return content
}