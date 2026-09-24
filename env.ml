(* Env module *)

module TAst = TypedAst

type binding =
  | Var of TAst.typ
  | Fun of TAst.funtype

type environment = (string * binding) list

let name_of (Ast.Ident {name}) = name

(* create an initial environment with the given functions defined *)
let make_env function_types =
  List.map (fun (name, ft) -> (name, Fun ft)) function_types

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ = (name_of sym, Var typ) :: env

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym =
  match List.assoc_opt (name_of sym) env with
  | Some binding -> binding
  | None -> failwith ("unbond: " ^ name_of sym)