(* -- Use this in your solution without modifications *)

module PBox = PrintBox

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
type location = {start_pos : Lexing.position; end_pos : Lexing.position}

let make_location (startp, endp) = {start_pos = startp; end_pos = endp}
let dummy_loc = {start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos}

module LocationOrder =
  struct
    type t = location
    let compare {start_pos = sp; end_pos = ep} {start_pos = sp'; end_pos = ep'} =
      match Stdlib.compare (sp.Lexing.pos_lnum) (sp'.Lexing.pos_lnum) with
      | 0 ->
        begin
          match Stdlib.compare (sp.Lexing.pos_cnum) (sp'.Lexing.pos_cnum) with
          | 0 ->
            begin
              match Stdlib.compare (ep.Lexing.pos_lnum) (ep'.Lexing.pos_lnum) with
              | 0 -> Stdlib.compare (ep.Lexing.pos_cnum) (ep'.Lexing.pos_cnum)
              | c -> c
            end
          | c -> c
        end
      | c -> c
  end

module LocationMap = Map.Make(LocationOrder)
module LocationSet = Set.Make(LocationOrder)

let location_style = PBox.Style.fg_color PBox.Style.Cyan

let location_to_tree ?(includefile = true) {start_pos; end_pos} =
  if includefile then
    if start_pos.pos_lnum = end_pos.pos_lnum then
      PBox.sprintf_with_style location_style "\"%s\" @ %d:%d-%d"
        start_pos.pos_fname start_pos.pos_lnum
        (start_pos.pos_cnum - start_pos.pos_bol)
        (end_pos.pos_cnum - end_pos.pos_bol)
    else
      PBox.sprintf_with_style location_style "\"%s\" @ %d:%d-%d:%d"
        start_pos.pos_fname start_pos.pos_lnum
        (start_pos.pos_cnum - start_pos.pos_bol)
        end_pos.pos_lnum
        (end_pos.pos_cnum - end_pos.pos_bol)
  else
    if start_pos.pos_lnum = end_pos.pos_lnum then
      PBox.sprintf_with_style location_style "@ %d:%d-%d"
        start_pos.pos_lnum
        (start_pos.pos_cnum - start_pos.pos_bol)
        (end_pos.pos_cnum - end_pos.pos_bol)
    else
      PBox.sprintf_with_style location_style "@ %d:%d-%d:%d"
        start_pos.pos_lnum
        (start_pos.pos_cnum - start_pos.pos_bol)
        end_pos.pos_lnum
        (end_pos.pos_cnum - end_pos.pos_bol)

let lexing_position_of_yojson =
  (let _tp_loc = "src/util/location.ml.lexing_position" in
   function
   | `Assoc field_yojsons as yojson ->
       let pos_fname_field = ref Ppx_yojson_conv_lib.Option.None
       and pos_lnum_field = ref Ppx_yojson_conv_lib.Option.None
       and pos_bol_field = ref Ppx_yojson_conv_lib.Option.None
       and pos_cnum_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "pos_fname" ->
                   (match Ppx_yojson_conv_lib.(!) pos_fname_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = string_of_yojson _field_yojson in
                        pos_fname_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "pos_lnum" ->
                   (match Ppx_yojson_conv_lib.(!) pos_lnum_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = int_of_yojson _field_yojson in
                        pos_lnum_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "pos_bol" ->
                   (match Ppx_yojson_conv_lib.(!) pos_bol_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = int_of_yojson _field_yojson in
                        pos_bol_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "pos_cnum" ->
                   (match Ppx_yojson_conv_lib.(!) pos_cnum_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = int_of_yojson _field_yojson in
                        pos_cnum_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) pos_fname_field),
                           (Ppx_yojson_conv_lib.(!) pos_lnum_field),
                           (Ppx_yojson_conv_lib.(!) pos_bol_field),
                           (Ppx_yojson_conv_lib.(!) pos_cnum_field))
                   with
                   | (Ppx_yojson_conv_lib.Option.Some pos_fname_value,
                      Ppx_yojson_conv_lib.Option.Some pos_lnum_value,
                      Ppx_yojson_conv_lib.Option.Some pos_bol_value,
                      Ppx_yojson_conv_lib.Option.Some pos_cnum_value) ->
                       {
                         Lexing.pos_fname = pos_fname_value;
                         Lexing.pos_lnum = pos_lnum_value;
                         Lexing.pos_bol = pos_bol_value;
                         Lexing.pos_cnum = pos_cnum_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) pos_fname_field)
                              Ppx_yojson_conv_lib.Option.None), "pos_fname");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) pos_lnum_field)
                             Ppx_yojson_conv_lib.Option.None), "pos_lnum");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) pos_bol_field)
                             Ppx_yojson_conv_lib.Option.None), "pos_bol");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) pos_cnum_field)
                             Ppx_yojson_conv_lib.Option.None), "pos_cnum")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> Lexing.position)
let yojson_of_lexing_position =
  (function
   | { Lexing.pos_fname = v_pos_fname; pos_lnum = v_pos_lnum; pos_bol = v_pos_bol;
       pos_cnum = v_pos_cnum } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_int v_pos_cnum in ("pos_cnum", arg) :: bnds in
       let bnds =
         let arg = yojson_of_int v_pos_bol in ("pos_bol", arg) :: bnds in
       let bnds =
         let arg = yojson_of_int v_pos_lnum in ("pos_lnum", arg) :: bnds in
       let bnds =
         let arg = yojson_of_string v_pos_fname in ("pos_fname", arg) :: bnds in
       `Assoc bnds : Lexing.position -> Ppx_yojson_conv_lib.Yojson.Safe.t)

let location_of_yojson =
  (let _tp_loc = "src/util/location.ml.location" in
   function
   | `Assoc field_yojsons as yojson ->
       let start_pos_field = ref Ppx_yojson_conv_lib.Option.None
       and end_pos_field = ref Ppx_yojson_conv_lib.Option.None
       and duplicates = ref []
       and extra = ref [] in
       let rec iter =
         function
         | (field_name, _field_yojson)::tail ->
             ((match field_name with
               | "start_pos" ->
                   (match Ppx_yojson_conv_lib.(!) start_pos_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = lexing_position_of_yojson _field_yojson in
                        start_pos_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | "end_pos" ->
                   (match Ppx_yojson_conv_lib.(!) end_pos_field with
                    | Ppx_yojson_conv_lib.Option.None ->
                        let fvalue = lexing_position_of_yojson _field_yojson in
                        end_pos_field :=
                          (Ppx_yojson_conv_lib.Option.Some fvalue)
                    | Ppx_yojson_conv_lib.Option.Some _ ->
                        duplicates := (field_name ::
                          (Ppx_yojson_conv_lib.(!) duplicates)))
               | _ ->
                   if
                     Ppx_yojson_conv_lib.(!)
                       Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
                   then
                     extra := (field_name :: (Ppx_yojson_conv_lib.(!) extra))
                   else ());
              iter tail)
         | [] -> () in
       (iter field_yojsons;
        (match Ppx_yojson_conv_lib.(!) duplicates with
         | _::_ ->
             Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields
               _tp_loc (Ppx_yojson_conv_lib.(!) duplicates) yojson
         | [] ->
             (match Ppx_yojson_conv_lib.(!) extra with
              | _::_ ->
                  Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields
                    _tp_loc (Ppx_yojson_conv_lib.(!) extra) yojson
              | [] ->
                  (match ((Ppx_yojson_conv_lib.(!) start_pos_field),
                           (Ppx_yojson_conv_lib.(!) end_pos_field))
                   with
                   | (Ppx_yojson_conv_lib.Option.Some start_pos_value,
                      Ppx_yojson_conv_lib.Option.Some end_pos_value) ->
                       { start_pos = start_pos_value; end_pos = end_pos_value
                       }
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                         _tp_loc yojson
                         [((Ppx_yojson_conv_lib.poly_equal
                              (Ppx_yojson_conv_lib.(!) start_pos_field)
                              Ppx_yojson_conv_lib.Option.None), "start_pos");
                         ((Ppx_yojson_conv_lib.poly_equal
                             (Ppx_yojson_conv_lib.(!) end_pos_field)
                             Ppx_yojson_conv_lib.Option.None), "end_pos")]))))
   | _ as yojson ->
       Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
         yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> location)
let yojson_of_location =
  (function
   | { start_pos = v_start_pos; end_pos = v_end_pos } ->
       let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
       let bnds =
         let arg = yojson_of_lexing_position v_end_pos in ("end_pos", arg) ::
           bnds in
       let bnds =
         let arg = yojson_of_lexing_position v_start_pos in
         ("start_pos", arg) :: bnds in
       `Assoc bnds : location -> Ppx_yojson_conv_lib.Yojson.Safe.t)
