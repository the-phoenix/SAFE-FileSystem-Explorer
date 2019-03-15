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

type ServerState = Idle | Loading | ServerError of string

type Model = {
    RootDirectory: UnixPath
    CurrentPath: UnixPath
    ValidationError : string option
    ServerState : ServerState
    DirectoryContent : FileSystemEntry array option
}

type Msg =
    | Increment
    | Decrement
    | InitialCountLoaded of Result<Counter, exn>
    | RootDirectoryChanged of UnixPath
    | CurrentPathChanged of UnixPath
    | GetDirContent of UnixPath
    | GotDirContent of FileSystemEntry array
    | ErrorMsg of exn

let initialCounter = fetchAs<Counter> "/api/init" (Decode.Auto.generateDecoder())
let getResponse path = promise {
    let endpoint = path |> System.Uri.EscapeDataString |> sprintf "api/dir/%s"
    let! resp = Fetch.fetchAs<FileSystemEntry []> endpoint (Decode.Auto.generateDecoder()) []

    return resp
}

let init () : Model * Cmd<Msg> =
    let initialModel = {
        RootDirectory = "";
        CurrentPath = "";
        ServerState = Idle;
        ValidationError = None;
        DirectoryContent = None;
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
            DirectoryContent = None
            ValidationError =
                if Validation.validatePath changed || changed.Trim() = "" then None
                else Some "Invalid directory path."
        }, Cmd.none
    | _, CurrentPathChanged changed ->
        { model with
            CurrentPath = changed
        }, Cmd.none
    | { ValidationError = None }, GetDirContent path ->
        { model with
            ServerState = Loading
            DirectoryContent = None
        }, Cmd.ofPromise getResponse path GotDirContent ErrorMsg
    | _, GotDirContent dirContent ->
        { model with
            ValidationError = None
            DirectoryContent = Some dirContent.[1..]
            CurrentPath = match dirContent.[0] with | Directory dir -> dir.FullPath | _ -> ""
            ServerState = Idle
        }, Cmd.none
    | _, ErrorMsg e -> { model with ServerState = ServerError e.Message }, Cmd.none
    | _ -> model, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let onEntryClicked (fsEntry: FileSystemEntry) =
        match fsEntry with
        | Directory dir ->
            dispatch (CurrentPathChanged dir.FullPath)
            dispatch (GetDirContent dir.FullPath)
        | File file -> ()
        // ignore

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
                                onKeyDown KeyCode.enter (fun _ -> model.RootDirectory |> GetDirContent |> dispatch )
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
                                          Button.OnClick (fun _ -> model.RootDirectory |> GetDirContent |> dispatch)
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
                match model with
                    | { DirectoryContent = None; ServerState = (Idle | Loading) } -> ()
                    | { ServerState = ServerError error } ->
                        yield
                            Field.div [] [
                                Tag.list [ Tag.List.HasAddons; Tag.List.IsCentered ] [
                                    Tag.tag [ Tag.Color Color.IsDanger; Tag.Size IsMedium ] [
                                        str error
                                    ]
                                ]
                            ]
                    | { DirectoryContent = Some content } ->

                        yield  Content.content [ Content.CustomClass "dirview-container"] [
                            Columns.columns []
                                [ Column.column [ Column.Width (Screen.WideScreen, Column.Is6)] [
                                    FixedSizePanel model.CurrentPath (
                                      viewDirectoryContent content ( model.RootDirectory = model.CurrentPath) onEntryClicked
                                    )
                                  ]
                                  Column.column [] [
                                    FixedSizePanel "Hey There" (str "I am the content")
                                  ]
                                ]
                        ]
          ]

          Footer.footer [ ]
            [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ safeComponents ] ] ]