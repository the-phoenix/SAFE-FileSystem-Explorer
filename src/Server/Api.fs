module Api

// open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open DataAccess
open Shared

open Saturn
open Giraffe
open Microsoft.AspNetCore.Http
open Saturn.ControllerHelpers
// open Shared

let getInitial next (ctx: HttpContext) = task {
    let! counter = getInitCounter()
    return! json counter next ctx
}

let getDirectoryList dirPath next (ctx:HttpContext) = task {
    let! directoryContent = getEntriesInDirectory(dirPath)

    match directoryContent with
    | DirResult contents->
        return! json contents next ctx
    | InvalidPath -> return! Response.badRequest ctx "Invalid path"
}

let getFileContent filePath next (ctx:HttpContext) = task {
    let! fileContent = getFileContent(filePath)

    match fileContent with
    | FileResult content ->
        return! json content next ctx
    | NotExists -> return! Response.badRequest ctx "File doesn't exist"
}

let webApp = router {
    get "/api/init" getInitial
    getf "/api/dir/%s" getDirectoryList
    getf "/api/file/%s" getFileContent
}