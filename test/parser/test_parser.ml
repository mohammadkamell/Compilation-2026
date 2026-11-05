(* Parser test driver, independent of semantic analysis and code generation.
     test_parser.exe parse FILE.dlp  : parse with our lexer and parser, print the AST
     test_parser.exe json  FILE.json : print the AST of a serialized (reference) AST
   Comparing the two outputs for the same program checks that our parser builds
   the same AST as the reference parser (locations are not printed). *)

let print_ast prog =
  PrintBox_text.output stdout (Pretty.program_to_tree prog);
  print_newline ()

let parse_file filename =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf filename;
  match Parser.program Lexer.token lexbuf with
  | prog -> close_in ch; print_ast prog; exit 0
  | exception Lexer.Error (loc, msg) ->
    close_in ch;
    Printf.eprintf "%s: lexer error: %s\n" (Errors.loc_to_string loc) msg;
    exit 1
  | exception Parser.Error ->
    close_in ch;
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.eprintf "%s:%d:%d: syntax error\n"
      p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1);
    exit 1

let () =
  match Sys.argv with
  | [| _; "parse"; file |] -> parse_file file
  | [| _; "json"; file |] ->
    print_ast (Deserializer.program_of_yojson (Yojson.Safe.from_file file))
  | _ ->
    prerr_endline "usage: test_parser.exe (parse FILE.dlp | json FILE.json)";
    exit 2