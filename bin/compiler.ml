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

let compile_prog_from_filename (_filename : string) : Ll.prog option =
  match open_in _filename with   
  | exception Sys_error err_message ->       
    Printf.eprintf "%s\n" err_message;       
    None
  | input_channel ->
    let lexbuff = Lexing.from_channel input_channel in
    let result =
    match Parser.program Lexer.token lexbuff with
      | exception Lexer.Error err_message ->         
      Printf.eprintf "Lexer error -> %s\n" err_message;         
      None     
      | exception Parser.Error -> 
        let position = lexbuff.Lexing.lex_curr_p in         
        Printf.eprintf "Syntax error at line %d, column %d\n"           
        position.Lexing.pos_lnum           
        (position.Lexing.pos_cnum - position.Lexing.pos_bol);         
        None     
      | ast -> compile_prog_from_ast ast 
     in   
      close_in input_channel;  
      result
