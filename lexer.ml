module Lexer = struct
  open Token

  exception Lexer_error of string

  let alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let numeric = "1234567890"

  let is_digit = function
      '0' .. '9' -> true
    | _ -> false
         
  let is_alpha = function
      'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false

  let is_alpha_numeric c = is_digit c || is_alpha c

  (* Helper function for lex_number *)
  let rec slurp_number acc in_s point =
    let addb c = Buffer.add_char acc c in
    match Stream.peek in_s with
      Some c ->
       if is_digit c
       then (addb (Stream.next in_s)  ; slurp_number acc in_s point)
       else if (c = '.')
       then if point
            then raise (Lexer_error "Multipointed number string")
            else (addb (Stream.next in_s);
                  slurp_number acc in_s true)
       else (Buffer.contents acc , in_s)
    | None -> (Buffer.contents acc) , in_s

  let lex_number in_s =
    let num_string, rest_s = slurp_number (Buffer.create 10) in_s false in
    Number (float_of_string num_string), rest_s

  (* Helper function for lex_identifier *)
  let rec slurp_ident acc in_s first =
    let addb c = Buffer.add_char acc c in
    match Stream.peek in_s with
      None -> (Buffer.contents acc), in_s
    | Some c -> if first
                then if is_alpha c
                     then (addb (Stream.next in_s) ;
                           slurp_ident acc in_s false)
                     else raise (Lexer_error "First character is not alpha")
                else if is_alpha_numeric c
                then (addb (Stream.next in_s);
                      slurp_ident acc in_s false)
                else (Buffer.contents acc), in_s
              
  let lex_identifier in_s =
    let ident_string, rest_s = slurp_ident (Buffer.create 10) in_s true in
    match ident_string with
      "def" -> Def, rest_s
    | "extern" -> Extern, rest_s
    | _ -> Ident ident_string, rest_s

  let rec slurp_comment in_s =
    match Stream.peek in_s with
      None -> in_s
    | Some '\n' -> (Stream.junk in_s; in_s)
    | Some _ -> (Stream.junk in_s; slurp_comment in_s)


  let rec lex in_s token_s =
    match Stream.peek in_s with
      None -> token_s
    | Some x -> match x with
                  ' ' | '\n' | '\r' | '\t' ->
                         Stream.junk in_s ; lex in_s token_s
                  | '#' -> let rest_s = slurp_comment in_s in lex rest_s token_s
                  | '0' .. '9' -> let tok, rest_s = lex_number in_s in
                                  lex rest_s (tok :: token_s)
                  | 'a' .. 'z' | 'A' .. 'Z' -> 
                     let tok, rest_s = lex_identifier in_s in
                     lex rest_s (tok :: token_s)
                  | _ ->  let tok = Stream.next in_s in
                          lex in_s (Kwd tok :: token_s)
                          
                          
end
                 
