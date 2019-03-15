[<AutoOpen>]

module ViewHelpers

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.FontAwesome

open Fulma
open Shared
open Fulma

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

let (|IsClickable|_|) fsEntry =
    match fsEntry with
    | Directory _ -> Some()
    | File file -> if file.ContentReadable then Some() else None

let showFSEntry onClick fsEntry  =
    li [
        Class (match fsEntry with | IsClickable -> "clickable" | _ -> "")
        OnClick (fun _ -> match fsEntry with | IsClickable -> onClick fsEntry | _ -> () )
    ] (
        match fsEntry with
        | File file -> [
            Fa.i [ Fa.Solid.FileAlt; Fa.IsLi ] []
            str file.Name]
        | Directory dir -> [
            Fa.i [ Fa.Solid.Folder; Fa.IsLi ] []
            str dir.Name]
    )

let viewDirectoryContent (dirContent: FileSystemEntry array) isRoot onClick =
    Field.div [] [
        Fa.ul [] (
            dirContent
            |> Array.filter (function | Directory { Name = ".." } -> false | _ -> true )
            |> Array.map (showFSEntry onClick)
        )
    ]
module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.

let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)