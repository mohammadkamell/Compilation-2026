(* Errors module: all semantic errors carry the location where they occur. *)
module TAst = TypedAst
module TPretty = TypedPretty
module Loc = Location

type error =
  | BinOpTypeError of {op : string; left : TAst.typ; right : TAst.typ; loc : Loc.location}
  | UnOpTypeError of {op : string; operand : TAst.typ; loc : Loc.location}
  | TypeMismatch of {context : string; expected : TAst.typ; actual : TAst.typ; loc : Loc.location}
  | UnboundIdentifier of {name : string; loc : Loc.location}
  | NotAFunction of {name : string; loc : Loc.location}
  | FunctionAsValue of {name : string; loc : Loc.location}
  | VoidAsValue of {name : string; loc : Loc.location}
  | ArgumentCountMismatch of {name : string; expected : int; actual : int; loc : Loc.location}
  | ArgumentTypeMismatch of
      {name : string; index : int; expected : TAst.typ; actual : TAst.typ; loc : Loc.location}
  | InvalidExpressionStatement of {loc : Loc.location}
  | BreakOutsideLoop of {loc : Loc.location}
  | ContinueOutsideLoop of {loc : Loc.location}
  | MissingReturn of {loc : Loc.location option}
  | DuplicateFunction of {name : string; loc : Loc.location}
  | DuplicateParameter of {name : string; loc : Loc.location}
  | MainMissing
  | MainBadSignature of {loc : Loc.location}
  | ReturnValueInVoid of {loc : Loc.location}
  | ReturnWithoutValue of {loc : Loc.location}

(* "file:line:column", using the start position of the location. *)
let loc_to_string (loc : Loc.location) : string =
  let p = loc.Loc.start_pos in
  Printf.sprintf "%s:%d:%d"
    p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1)

let t = TPretty.typ_to_string

let message = function
  | BinOpTypeError {op; left; right; _} ->
    Printf.sprintf "operator '%s' cannot be applied to operands of type %s and %s"
      op (t left) (t right)
  | UnOpTypeError {op; operand; _} ->
    Printf.sprintf "operator '%s' cannot be applied to an operand of type %s" op (t operand)
  | TypeMismatch {context; expected; actual; _} ->
    Printf.sprintf "type mismatch in %s: expected %s but found %s" context (t expected) (t actual)
  | UnboundIdentifier {name; _} ->
    Printf.sprintf "unbound identifier '%s'" name
  | NotAFunction {name; _} ->
    Printf.sprintf "'%s' is a variable, not a function, and cannot be called" name
  | FunctionAsValue {name; _} ->
    Printf.sprintf "function '%s' cannot be used as a value" name
  | VoidAsValue {name; _} ->
    Printf.sprintf "call to '%s' returns void and cannot be used as a value" name
  | ArgumentCountMismatch {name; expected; actual; _} ->
    Printf.sprintf "function '%s' expects %d argument(s) but is given %d" name expected actual
  | ArgumentTypeMismatch {name; index; expected; actual; _} ->
    Printf.sprintf "argument %d of '%s' should have type %s but has type %s"
      index name (t expected) (t actual)
  | InvalidExpressionStatement _ ->
    "only assignments and function calls can be used as statements"
  | BreakOutsideLoop _ -> "break statement outside of a loop"
  | ContinueOutsideLoop _ -> "continue statement outside of a loop"
  | MissingReturn _ -> "function must return a value on all paths"   
  | DuplicateFunction {name; _} ->   Printf.sprintf "function '%s' is declared more than once" name   
  | DuplicateParameter {name; _} ->   Printf.sprintf "parameter '%s' is declared more than once" name   
  | MainMissing -> "the program must contain a function 'main'"   
  | MainBadSignature _ -> "main must have return type int and no arguments"   
  | ReturnValueInVoid _ -> "a void function cant return a value"   
  | ReturnWithoutValue _ -> "a non void function must return a value"
  

let location_of = function
  | BinOpTypeError { loc; _ }
  | UnOpTypeError { loc; _ }
  | TypeMismatch { loc; _ }
  | UnboundIdentifier { loc; _ }
  | NotAFunction { loc; _ }
  | FunctionAsValue { loc; _ }
  | VoidAsValue { loc; _ }
  | ArgumentCountMismatch { loc; _ }
  | ArgumentTypeMismatch { loc; _ }
  | InvalidExpressionStatement { loc }
  | BreakOutsideLoop { loc }
  | ContinueOutsideLoop { loc }
  | DuplicateFunction { loc; _ }
  | DuplicateParameter { loc; _ }
  | MainBadSignature { loc }
  | ReturnValueInVoid { loc }
  | ReturnWithoutValue { loc } ->
      Some loc
  | MissingReturn { loc } -> loc
  | MainMissing -> None

(* e.g. "prog.dlp:3:5: error: unbound identifier 'y'" *)
let error_to_string err =
  match location_of err with
  | Some loc -> Printf.sprintf "%s: error: %s" (loc_to_string loc) (message err)
  | None -> Printf.sprintf "error: %s" (message err)