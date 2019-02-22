module Token = struct
  type token =
    | Def | Extern
    | Ident of string | Number of float
    | Kwd of char
end
