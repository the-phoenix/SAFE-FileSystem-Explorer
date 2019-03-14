namespace Shared
open System.IO

type Counter = { Value : int }
type FileSystemInfo =
    | File of FileInfo
    | Directory of DirectoryInfo
and FileInfo = { Name: string; ContentReadable: bool; }
and DirectoryInfo = { Name: string; }

type DirectoryListResponse =
    | InvalidPath
    | DirResult of FileSystemInfo []