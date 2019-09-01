namespace Shared

type FileItem =
    | Directory of {| FullPath: string; Children: FileItem list |}
    | File of {| FullPath: string; Size: int |}

type ServerMsg = unit

type ClientMsg = LoadRoot of FileItem list
