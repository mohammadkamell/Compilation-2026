(* Env module *)

module TAst = TypedAst

type binding =
  | Var of TAst.typ
  | Fun of TAst.funtype

type environment = {
  bindings : (string * binding) list;
  loop_depth : int;
}

let name_of (Ast.Ident {name}) = name

(* create an initial environment with the given functions defined *)
let make_env function_types =
  { bindings = List.map (fun (name, ft) -> (name, Fun ft)) function_types;
    loop_depth = 0 }

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ =
  { env with bindings = (name_of sym, Var typ) :: env.bindings }

(* Environments are immutable: discarding a loop's environment restores both
   its enclosing scope and its enclosing loop depth. *)
let enter_loop env = { env with loop_depth = env.loop_depth + 1 }

let is_inside_loop env = env.loop_depth > 0

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym =
  match List.assoc_opt (name_of sym) env.bindings with
  | Some binding -> binding
  | None -> failwith ("unbond: " ^ name_of sym)
