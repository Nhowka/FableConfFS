namespace Shared

type FileItem =
    | Directory of {| FullPath: string; Children: FileItem list |}
    | File of {| FullPath: string; Size: int |}


type ServerMsg =
    | RequestDownload of string
    | RequestUpload of string
    | UploadSmallFile of string * string
    | CreateFolder of string

type ClientMsg =
    | LoadRoot of FileItem list
    | FileRenamed of {| OldName: string; NewName: string |}
    | FileChanged of {| FullPath: string; Size: int |}
    | FileCreated of {| FullPath: string; Size: int |}
    | FolderCreated of string
    | FileDeleted of string
    | SetGuid of System.Guid
    | UploaderReady of string * System.Guid
    | DownloadSmallFile of string * string
    | DownloadBigFile of string * System.Guid