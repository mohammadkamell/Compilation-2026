%{
  open Ast
  module Loc = Location
%}

// end of file
%token EOF
// integer literals
%token <int64> INT_LIT
// booleans
%token TRUE FALSE
// length operation; for arrays and strings
%token LENGTHOF
// arithmetic operations
%token PLUS MINUS MUL DIV REM
// comparison operators
%token LT LE GT GE
// logical operations
%token LOR LAND LNOT
// equality
%token EQ NEQ
// assignment
%token ASSIGN
// punctuation
%token COLON COMMA SEMICOLON
// accessors
%token DOT LBRACKET RBRACKET
// braces
%token LBRACE RBRACE
// parentheses
%token LPAREN RPAREN
// identifiers
%token <string> IDENT
// keywords
%token NIL VAR IF ELSE WHILE FOR BREAK CONTINUE RETURN NEW
// types
%token INT BOOL STRING BYTE VOID RECORD

%start <Ast.program> program

%%

program:
  | EOF { [] }