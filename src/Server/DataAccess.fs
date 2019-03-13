module DataAccess

open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open Shared

let getInitCounter() : Task<Counter> = task { return { Value = 42 } }