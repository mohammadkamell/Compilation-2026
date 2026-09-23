(* Env module *)

exception Unimplemented (* your code should eventually compile without this exception *)

type environment = | (* to be defined *)

(* create an initial environment with the given functions defined *)
let make_env function_types = raise Unimplemented

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ = raise Unimplemented

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym = raise Unimplemented