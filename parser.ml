module Parser = struct
  open Token
  open Ast
     
  exception Parse_error of string
            
  let (<|>) p1 p2 =
    fun s ->  let (r1, s1) = p1 s in
              let (r2, s2) = p2 s1 in
              (r1 @ r2, s2)

  let rec parse_primary s =
    let next_token = Stream.next s in
    match next_token with
      Token.Number n -> (Ast.Number n, s)
    | Token.Ident str -> (Ast.Variable str, s)
    | Token.Kwd c -> parse_kwd c s
    | Token.Def -> parse_def s
    | Token.Extern -> parse_extern s
  and parse_kwd c s = if c = '('
                      then let prim, rest_s = parse_primary s in
                           match Stream.next rest_s with
                             Token.Kwd ')' -> prim, rest_s
                           | _ -> raise (Parse_error "Not closing paranthesis")
                      else raise @@
                             Parse_error ("Unknown Keyword"^String.make 1 c)
  and parse_def s = raise @@ Parse_error "Unimplemented"
  and parse_extern s = raise @@ Parse_error "Unimplemented"

end 
