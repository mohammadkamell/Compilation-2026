(* -- Use this in your solution without modifications *)

type location = {start_pos : Lexing.position; end_pos : Lexing.position}

val dummy_loc : location
val make_location : Lexing.position * Lexing.position -> location

module LocationMap : Map.S with type key = location
module LocationSet : Set.S with type elt = location

val location_to_tree : ?includefile:bool -> location -> PrintBox.t
val location_of_yojson : Yojson.Safe.t -> location
val yojson_of_location : location -> Yojson.Safe.t
