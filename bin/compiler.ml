let compile_prog_from_ast (prog : Ast.program) : Ll.prog option =
  match Semant.typecheck_prog prog with
  | exception Semant.TypeError errors ->
      List.iter (fun msg -> Printf.eprintf "%s\n" msg) errors;
      None
  | exception Failure msg ->
    Printf.eprintf "%s\n" msg ; 
    None
  | typed_prog ->
      let ll_prog = Codegen.codegen_prog typed_prog in
      Some ll_prog

let compile_prog_from_filename (filename : string) : Ll.prog option =
  try
    let ch = open_in filename in
    let lexbuf = Lexing.from_channel ch in
    Lexing.set_filename lexbuf filename;
    let prog =
      Fun.protect
        ~finally:(fun () -> close_in ch)
        (fun () -> Parser.program Lexer.token lexbuf)
    in
    compile_prog_from_ast prog
  with
  | Lexer.Error (loc, msg) ->
    Printf.eprintf "%s: %s\n" (PrintBox_text.to_string (Location.location_to_tree loc)) msg;
    None
  | Parser.Error ->
    Printf.eprintf "Syntax error in %s\n" filename;
    None