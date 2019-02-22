exception Parse_error of string

type langAtom =
  | Expr of Ast.expr
  | Proto of Ast.proto
  | Func of Ast.func
             
let (<|>) p1 p2 =
  fun s ->  let (r1, s1) = p1 s in
            let (r2, s2) = p2 s1 in
            (r1 @ r2, s2)

let parse_number s =
  match Stream.next s with
    Token.Number n -> ([Expr(Ast.Number n)], s)
  | _ -> raise @@ Parse_error "Expecting Number token"

let parse_variable s =
  match Stream.next s with
    Token.Ident id_string ->
     ([Expr(Ast.Variable id_string)], s)
  | _ -> raise @@ Parse_error "Expecting identifier"

let parse_bin_exp s = 
  let get_bin_op s = match Stream.next s with
      Token.Kwd c -> c
  in Ast.
                   
let parse_def =
  Parse (fun s -> match Stream.next s with
                    Token.Def -> 
    )
let rec parse_primary s =
    let next_token = Stream.next s in
    match next_token with
      Token.Number n -> (Ast.Number n, s)
    | Token.Ident str -> (Ast.Variable str, s)
    | Token.Kwd c -> parse_kwd c s
    | Token.Def -> parse_def s
    | Token.Extern -> parse_extern s
and parse_kwd c s = if c = '('
                    then parse_primary s
                    else raise @@
                           Parse_error ("Unknown Keyword"^String.make 1 c)
and parse_def s = 
