{
open Parser

exception Error of Location.location * string

let error lexbuf msg =
  let loc = Location.make_location (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
  raise (Error (loc, msg))

let string_of_token = function
  | EOF -> "EOF"
  | INT_LIT i -> Printf.sprintf "INT_LIT(%Ld)" i
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | LENGTHOF -> "LENGTHOF"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | REM -> "REM"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | LOR -> "LOR"
  | LAND -> "LAND"
  | LNOT -> "LNOT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | ASSIGN -> "ASSIGN"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | DOT -> "DOT"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | NIL -> "NIL"
  | VAR -> "VAR"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | RETURN -> "RETURN"
  | NEW -> "NEW"
  | INT -> "INT"
  | BOOL -> "BOOL"
  | STRING -> "STRING"
  | BYTE -> "BYTE"
  | VOID -> "VOID"
  | RECORD -> "RECORD"
}

let whitespace = [' ' '\t']
let newline = "\r\n" | '\n'
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*

rule token = parse
  | whitespace+   { token lexbuf }
  | newline       { Lexing.new_line lexbuf; token lexbuf }
  | "//"          { single_line_comment lexbuf }
  | "/*"          { multi_line_comment 1 lexbuf }

  (* Keywords *)
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "nil"         { NIL }
  | "var"         { VAR }
  | "if"          { IF }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "for"         { FOR }
  | "break"       { BREAK }
  | "continue"    { CONTINUE }
  | "return"      { RETURN }
  | "int"         { INT }
  | "byte"        { BYTE }
  | "bool"        { BOOL }
  | "string"      { STRING }
  | "void"        { VOID }
  | "record"      { RECORD }
  | "new"         { NEW }
  | "length_of"   { LENGTHOF }

  (* Literals *)
  | digit+ as s {
      try
        INT_LIT (Int64.of_string s)
      with Failure _ ->
        error lexbuf (Printf.sprintf "Integer literal '%s' out of range." s)
    }

  (* Identifiers *)
  | ident as id   { IDENT id }

  (* Operators *)
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { MUL }
  | '/'           { DIV }
  | '%'           { REM }
  | "<="          { LE }
  | '<'           { LT }
  | ">="          { GE }
  | '>'           { GT }
  | "=="          { EQ }
  | "!="          { NEQ }
  | "||"          { LOR }
  | "&&"          { LAND }
  | '!'           { LNOT }
  | '='           { ASSIGN }
  | '.'           { DOT }

  (* Punctuation *)
  | ':'           { COLON }
  | ','           { COMMA }
  | ';'           { SEMICOLON }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }

  (* End of file *)
  | eof           { EOF }

  (* Illegal character *)
  | _ as c {
      error lexbuf (Printf.sprintf "unexpected character '%c' encounterd." c)
    }

and single_line_comment = parse
  | newline       { Lexing.new_line lexbuf; token lexbuf }
  | eof           { EOF }
  | _             { single_line_comment lexbuf }

and multi_line_comment depth = parse
  | "/*"          { multi_line_comment (depth + 1) lexbuf }
  | "*/"          { if depth = 1 then token lexbuf else multi_line_comment (depth - 1) lexbuf }
  | newline       { Lexing.new_line lexbuf; multi_line_comment depth lexbuf }
  | eof           { error lexbuf "file ended before multi-line comment ended." }
  | _             { multi_line_comment depth lexbuf }

{
let tokenize_from_lexbuf lexbuf =
  let rec loop acc =
    let tok = token lexbuf in
    if tok = EOF then List.rev (tok :: acc)
    else loop (tok :: acc)
  in
  loop []

let tokenize_file filename =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf filename;
  Fun.protect
    ~finally:(fun () -> close_in ch)
    (fun () -> tokenize_from_lexbuf lexbuf)
}
