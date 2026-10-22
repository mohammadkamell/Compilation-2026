(* Test suite for the Dolphin Lexer (Phase 3 - Task 1) *)

open Parser

let token_equal (t1 : Parser.token) (t2 : Parser.token) =
  t1 = t2

let test_tokens () =
  let input = {|
    // Single-line comment
    /* Multi-line comment */
    /* Nested /* multi-line /* comment */ */ */
    var x : int = 42;
    var y : bool = true;
    var z = false;
    if (x <= 100 && y || !z) {
      x = x + 1 - 2 * 3 / 4 % 5;
      return x;
    } else {
      while (x > 0) {
        break;
        continue;
      }
      for (var i : int = 0; i < 10; i = i + 1) {
        nil;
      }
    }
    byte string void record new length_of [ ] == != >=
  |} in
  let lexbuf = Lexing.from_string input in
  let tokens = Lexer.tokenize_from_lexbuf lexbuf in
  let expected = [
    VAR; IDENT "x"; COLON; INT; ASSIGN; INT_LIT 42L; SEMICOLON;
    VAR; IDENT "y"; COLON; BOOL; ASSIGN; TRUE; SEMICOLON;
    VAR; IDENT "z"; ASSIGN; FALSE; SEMICOLON;
    IF; LPAREN; IDENT "x"; LE; INT_LIT 100L; LAND; IDENT "y"; LOR; LNOT; IDENT "z"; RPAREN; LBRACE;
    IDENT "x"; ASSIGN; IDENT "x"; PLUS; INT_LIT 1L; MINUS; INT_LIT 2L; MUL; INT_LIT 3L; DIV; INT_LIT 4L; REM; INT_LIT 5L; SEMICOLON;
    RETURN; IDENT "x"; SEMICOLON;
    RBRACE; ELSE; LBRACE;
    WHILE; LPAREN; IDENT "x"; GT; INT_LIT 0L; RPAREN; LBRACE;
    BREAK; SEMICOLON;
    CONTINUE; SEMICOLON;
    RBRACE;
    FOR; LPAREN; VAR; IDENT "i"; COLON; INT; ASSIGN; INT_LIT 0L; SEMICOLON; IDENT "i"; LT; INT_LIT 10L; SEMICOLON; IDENT "i"; ASSIGN; IDENT "i"; PLUS; INT_LIT 1L; RPAREN; LBRACE;
    NIL; SEMICOLON;
    RBRACE;
    RBRACE;
    BYTE; STRING; VOID; RECORD; NEW; LENGTHOF; LBRACKET; RBRACKET; EQ; NEQ; GE;
    EOF
  ] in
  assert (List.length tokens = List.length expected);
  List.iter2 (fun actual exp ->
    if not (token_equal actual exp) then
      failwith (Printf.sprintf "Mismatch: expected %s, got %s"
        (Lexer.string_of_token exp) (Lexer.string_of_token actual))
  ) tokens expected;
  Printf.printf "[PASS] All tokens, keywords, and comments recognized correctly.\n"

let test_line_numbers () =
  let input = "a\nb\r\nc\n// comment\nd\n/* comment\nwith\nnewlines */\ne" in
  let lexbuf = Lexing.from_string input in
  let rec collect () =
    let tok = Lexer.token lexbuf in
    let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
    if tok = EOF then []
    else (tok, line) :: collect ()
  in
  let items = collect () in
  let expected = [
    (IDENT "a", 1);
    (IDENT "b", 2);
    (IDENT "c", 3);
    (IDENT "d", 5);
    (IDENT "e", 9);
  ] in
  List.iter2 (fun (act_tok, act_ln) (exp_tok, exp_ln) ->
    if not (token_equal act_tok exp_tok) || act_ln <> exp_ln then
      failwith (Printf.sprintf "Mismatch: expected (%s, line %d), got (%s, line %d)"
        (Lexer.string_of_token exp_tok) exp_ln
        (Lexer.string_of_token act_tok) act_ln)
  ) items expected;
  Printf.printf "[PASS] Line numbering maintained correctly across Unix/Windows newlines and comments.\n"

let test_int_bounds () =
  (* Max int64: 9223372036854775807 *)
  let input = "9223372036854775807" in
  let lexbuf = Lexing.from_string input in
  let tok = Lexer.token lexbuf in
  assert (tok = INT_LIT 9223372036854775807L);

  (* Out of range int64 *)
  let input_overflow = "9223372036854775808" in
  let lexbuf_overflow = Lexing.from_string input_overflow in
  let overflow_caught =
    try
      let _ = Lexer.token lexbuf_overflow in false
    with Lexer.Error (_, _) -> true
  in
  assert overflow_caught;
  Printf.printf "[PASS] Integer literal range checks working properly.\n"

let test_illegal_char () =
  let input = "var @x = 1;" in
  let lexbuf = Lexing.from_string input in
  let _ = Lexer.token lexbuf in (* var *)
  let caught =
    try
      let _ = Lexer.token lexbuf in false
    with Lexer.Error (loc, msg) ->
      assert (msg = "unexpected character '@' encounterd.");
      let col = loc.start_pos.pos_cnum - loc.start_pos.pos_bol in
      assert (col = 4);
      true
  in
  assert caught;
  Printf.printf "[PASS] Illegal character error reporting working properly.\n"

let test_unclosed_comment () =
  let input = "/* unclosed comment" in
  let lexbuf = Lexing.from_string input in
  let caught =
    try
      let _ = Lexer.token lexbuf in false
    with Lexer.Error (_, msg) ->
      assert (msg = "file ended before multi-line comment ended.");
      true
  in
  assert caught;
  Printf.printf "[PASS] Unclosed multi-line comment detected.\n"

let () =
  if Array.length Sys.argv > 1 then begin
    let filename = Sys.argv.(1) in
    Printf.printf "Tokenizing file: %s\n" filename;
    let ch = open_in filename in
    let lexbuf = Lexing.from_channel ch in
    Lexing.set_filename lexbuf filename;
    let rec print_all () =
      let tok = Lexer.token lexbuf in
      let start_p = Lexing.lexeme_start_p lexbuf in
      let end_p = Lexing.lexeme_end_p lexbuf in
      let loc = Location.make_location (start_p, end_p) in
      Printf.printf "  %-25s %s\n" (Lexer.string_of_token tok) (PrintBox_text.to_string (Location.location_to_tree loc));
      if tok <> Parser.EOF then print_all ()
    in
    print_all ();
    close_in ch
  end else begin
    Printf.printf "=== Running Lexer Tests ===\n";
    test_tokens ();
    test_line_numbers ();
    test_int_bounds ();
    test_illegal_char ();
    test_unclosed_comment ();
    Printf.printf "=== All Lexer Tests Passed! ===\n"
  end
