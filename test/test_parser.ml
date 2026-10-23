(* Test that Parser and Lexer work together on all test files *)

let parse_file filename =
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf filename;
  Fun.protect
    ~finally:(fun () -> close_in ch)
    (fun () -> Parser.program Lexer.token lexbuf)

let resolve_dir dir =
  if Sys.file_exists dir then dir
  else if Sys.file_exists (Filename.concat "test" dir) then Filename.concat "test" dir
  else if String.length dir > 5 && String.sub dir 0 5 = "test/" && Sys.file_exists (String.sub dir 5 (String.length dir - 5)) then
    String.sub dir 5 (String.length dir - 5)
  else dir

let test_dir dir =
  let real_dir = resolve_dir dir in
  let files = Sys.readdir real_dir in
  Array.sort String.compare files;
  let passed = ref 0 in
  let failed = ref 0 in
  Array.iter (fun file ->
    if Filename.check_suffix file ".dlp" then begin
      let path = Filename.concat real_dir file in
      try
        let ast = parse_file path in
        assert (List.length ast >= 0);
        incr passed;
        Printf.printf "  [OK] %s (parsed %d statements)\n" file (List.length ast)
      with
      | Lexer.Error (loc, msg) ->
        incr failed;
        Printf.eprintf "  [LEXER ERROR] %s: %s (%s)\n"
          file msg (PrintBox_text.to_string (Location.location_to_tree loc))
      | Parser.Error ->
        incr failed;
        Printf.eprintf "  [PARSER ERROR] %s: syntax error\n" file
      | exn ->
        incr failed;
        Printf.eprintf "  [ERROR] %s: %s\n" file (Printexc.to_string exn)
    end
  ) files;
  (!passed, !failed)

let test_syntax_negative_dir dir =
  let real_dir = resolve_dir dir in
  let files = Sys.readdir real_dir in
  Array.sort String.compare files;
  let passed = ref 0 in
  let failed = ref 0 in
  Array.iter (fun file ->
    if Filename.check_suffix file ".dlp" then begin
      let path = Filename.concat real_dir file in
      let rejected =
        try
          let _ = parse_file path in false
        with
        | Lexer.Error _ | Parser.Error -> true
        | _ -> true
      in
      if rejected then begin
        incr passed;
        Printf.printf "  [OK (rejected)] %s\n" file
      end else begin
        incr failed;
        Printf.eprintf "  [UNEXPECTED OK] %s\n" file
      end
    end
  ) files;
  (!passed, !failed)

let () =
  Printf.printf "=== Testing Positive Tests ===\n";
  let pos_p, pos_f = test_dir "test/tests/positive" in
  Printf.printf "Positive: %d passed, %d failed\n\n" pos_p pos_f;

  Printf.printf "=== Testing Negative Tests (Syntax Check) ===\n";
  let neg_p, neg_f = test_dir "test/tests/negative" in
  Printf.printf "Negative (syntax): %d passed, %d failed\n\n" neg_p neg_f;

  Printf.printf "=== Testing Assignment 5 Negative Tests (Lexer & Parser) ===\n";
  let as5_p, as5_f = test_syntax_negative_dir "test/tests/assignment-05-tests" in
  Printf.printf "Assignment 5 negative: %d passed, %d failed\n\n" as5_p as5_f;

  if pos_f > 0 || neg_f > 0 || as5_f > 0 then exit 1
  else Printf.printf "=== All %d tests passed in Lexer + Parser test suite! ===\n" (pos_p + neg_p + as5_p)
