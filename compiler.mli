(* -- Use this in your solution without modifications *)

val compile_prog_from_ast : Ast.program -> Ll.prog option

val compile_prog_from_filename : string -> Ll.prog option