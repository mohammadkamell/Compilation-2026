let compile_prog_from_ast (prog : Ast.program) : Ll.prog option =
  match Semant.typecheck_prog prog with
  | exception Semant.TypeError errors ->
      List.iter (fun msg -> Printf.eprintf "%s\n" msg) errors;
      None
  | typed_prog ->
      let ll_prog = Codegen.codegen_prog typed_prog in
      Some ll_prog

let compile_prog_from_filename (_filename : string) : Ll.prog option =
  Printf.eprintf "compile_prog_from_filename: not implemented for Phase 1\n";
  None