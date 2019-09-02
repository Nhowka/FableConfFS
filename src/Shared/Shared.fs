namespace Shared

type FileItem =
    | Directory of {| FullPath: string; Children: FileItem list |}
    | File of {| FullPath: string; Size: int |}

type ServerMsg = unit

type ClientMsg =
    | LoadRoot of FileItem list
    | FileRenamed of {| OldName: string; NewName: string |}
    | FileChanged of {| FullPath: string; Size: int |}
    | FileCreated of {| FullPath: string; Size: int |}
    | FolderCreated of string
    | FileDeleted of string

