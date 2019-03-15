namespace Shared
open System.IO

type UnixPath = string
type Counter = { Value : int }
type FileSystemEntry =
    | File of FileInfo
    | Directory of DirectoryInfo

and FileInfo = { Name: string; ContentReadable: bool; FullPath: UnixPath; }
and DirectoryInfo = { Name: string; FullPath: UnixPath; }

type DirectoryListResponse =
    | InvalidPath
    | DirResult of FileSystemEntry []

module Validation =
    // open System.Text.RegularExpressions
    let validatePath path =
        true
        // TODO: Should update with correct regular expression
        // Regex.IsMatch(path, @"^([\w]\: (\\\\|\\) ([a-zA-Z0-9\-\_\\]+))$")