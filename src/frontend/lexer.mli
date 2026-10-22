exception Error of Location.location * string

val token : Lexing.lexbuf -> Parser.token
val string_of_token : Parser.token -> string
val tokenize_from_lexbuf : Lexing.lexbuf -> Parser.token list
val tokenize_file : string -> Parser.token list
