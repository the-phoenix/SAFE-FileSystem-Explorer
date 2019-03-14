[<AutoOpen>]

module ViewHelpers

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import

open Fulma
open Shared

let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
           ]

    p [ ]
        [ strong [] [ str "Directory Explorer" ]
          str " powered by: "
          components ]

let btn txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let showFSEntry fsEntry =
    li [] [
        str (match fsEntry with
            | File file -> file.Name
            | Directory dir -> dir.Name
        )
    ]

let viewDirectoryContent (dirContent: FileSystemEntry array) =
    Browser.console.log("dirContent!", dirContent)
    Field.div [] [
        // ul [] (dirContent |> Array.map (function | File f -> str f.Name | Directory f -> str f.Name))
        ul [] (dirContent |> Array.map (showFSEntry))
    ]