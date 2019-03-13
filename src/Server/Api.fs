module Api

// open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open DataAccess

open Saturn
open Giraffe
// open Shared

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let! counter = getInitCounter()
            return! json counter next ctx
        })
}