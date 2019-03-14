module App

open Elmish

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open Fable.FontAwesome
open Thoth.Json

open Shared


open Fulma
open System.IO

type UnixPath = string
type ServerState = Idle | Loading | ServerError of string

type Model = {
    RootDirectory: UnixPath
    CurrentPath: UnixPath
    ValidationError : string option
    ServerState : ServerState
    DirectoryContent : FileSystemEntry array
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Result<Counter, exn>
    | RootDirectoryChanged of UnixPath
    | CurrentPathChanged of UnixPath
    | GetDirContent
    | GotDirContent of FileSystemEntry array
    | ErrorMsg of exn

let initialCounter = fetchAs<Counter> "/api/init" (Decode.Auto.generateDecoder())
let getResponse path = promise {
    let endpoint = path |> System.Uri.EscapeDataString |> sprintf "api/dir/%s"
    let! resp = Fetch.fetchAs<FileSystemEntry []> endpoint (Decode.Auto.generateDecoder()) []

    return resp
}

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = {
        RootDirectory = "";
        CurrentPath = "";
        ServerState = Idle;
        ValidationError = None;
        DirectoryContent = [||];
    }

    let loadCountCmd =
        Cmd.ofPromise
            initialCounter
            []
            (Ok >> InitialCountLoaded)
            (Error >> InitialCountLoaded)
    initialModel, loadCountCmd



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match model, msg with
    //| Some counter, Increment ->
    //    let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
    //    nextModel, Cmd.none
    //| Some counter, Decrement ->
    //    let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
    //    nextModel, Cmd.none
    //| _, InitialCountLoaded (Ok initialCount)->
        //let nextModel = { currentModel with Counter = Some initialCount }
        //nextModel, Cmd.none
    | _, RootDirectoryChanged changed ->
        { model with
            RootDirectory = changed
            ValidationError =
                if Validation.validatePath changed || changed.Trim() = "" then None
                else Some "Invalid directory path."
        }, Cmd.none
    | _, CurrentPathChanged changed ->
        { model with
            CurrentPath = changed
        }, Cmd.none
    | { ValidationError = None; RootDirectory = path }, GetDirContent ->
        { model with ServerState = Loading }, Cmd.ofPromise getResponse path GotDirContent ErrorMsg
    | _, GotDirContent dirContent ->
        { model with
            ValidationError = None
            DirectoryContent = dirContent
            CurrentPath = model.RootDirectory
            ServerState = Idle
        }, Cmd.none
    | _, ErrorMsg e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _ -> model, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Directory Explorer" ] ] ]

          Container.container []
            [
                yield Field.div [] [
                    Label.label [] [ str "Root Directory" ]
                    Control.div [ Control.HasIconLeft; Control.HasIconRight ] [
                        Input.text [
                            Input.Placeholder "Ex: /Users"
                            Input.Value model.RootDirectory
                            Input.Color (if model.ValidationError.IsSome then Color.IsDanger else Color.IsSuccess)
                            Input.Props [
                                OnChange (fun ev -> !!ev.target?value |> RootDirectoryChanged |> dispatch )
                            ]
                        ]
                        Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [
                            Fa.i [ Fa.Solid.FolderOpen ] [ ]
                        ]
                        (match (model.ValidationError, model.RootDirectory.Trim() ) with
                        | (Some _, _) ->
                            Icon.icon [
                                Icon.Size IsSmall
                                Icon.IsRight
                                Icon.Modifiers [Modifier.TextColor IsDanger]
                            ] [
                                Fa.i [ Fa.Solid.Exclamation ] [ ]
                            ]
                        | (_, "") | (None, _) ->
                            Icon.icon [
                                Icon.Size IsSmall
                                Icon.IsRight
                                Icon.Modifiers [Modifier.TextColor IsSuccess]
                            ] [
                                Fa.i [ Fa.Solid.Check ] [ ]
                            ]
                        )
                    ]
                    Help.help
                       [ Help.Color (if model.ValidationError.IsNone then IsSuccess else IsDanger) ]
                       [ str (model.ValidationError |> Option.defaultValue "") ]
                ]
                yield
                    Field.div [ Field.IsGrouped ] [
                        Level.level [ ] [
                            Level.left [] [
                                Level.item [] [
                                    Button.button
                                        [ Button.IsFullWidth
                                          Button.Color IsPrimary
                                          Button.OnClick (fun _ -> dispatch GetDirContent)
                                          Button.Disabled (model.ValidationError.IsSome)
                                          Button.IsLoading (model.ServerState = ServerState.Loading) ]
                                        [ str "Submit" ] ]
                                Level.item [] [
                                    Button.button
                                        [ Button.Color IsPrimary
                                          //Button.OnClick (fun _ -> dispatch ClearResult)
                                          //Button.Disabled model.Report.IsNone
                                        ]
                                        [ str "Clear Result" ] ] ] ]

                    ]
                yield  Content.content [ Content.CustomClass "dirview-container"] [
                    viewDirectoryContent model.DirectoryContent
                ]
          ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]