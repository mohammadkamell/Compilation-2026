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

// Precedence, from lowest to highest
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE            // dangling else: else binds to the innermost if
%right ASSIGN
%left LOR
%left LAND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE
%left PLUS MINUS
%left MUL DIV REM
%nonassoc UNARY

%%

// ---------------------------------------------------------------------------
// Programs and functions
// ---------------------------------------------------------------------------

program:
  | fs = nonempty_list(fundecl) EOF { fs }

fundecl:
  | ret = rettyp name = ident LPAREN params = separated_list(COMMA, param) RPAREN
      LBRACE body = list(statement) RBRACE
      { FunDecl {ret; funname = name; params; body; loc = Loc.make_location $loc} }

rettyp:
  | VOID    { Void {loc = Loc.make_location $loc} }
  | t = typ { RetTyp t }

param:
  | id = ident COLON t = typ { (id, t) }

// ---------------------------------------------------------------------------
// Statements
// Statement-level expressions may be comma expressions (comma_expr).
// ---------------------------------------------------------------------------

statement:
  | db = decl_block SEMICOLON
      { VarDeclStm db }
  | e = option(comma_expr) SEMICOLON                         // phase 4: comma
      { ExprStm {expr = e; loc = Loc.make_location $loc} }
  | IF LPAREN c = comma_expr RPAREN t = statement %prec LOWER_THAN_ELSE   // phase 4: comma
      { IfThenElseStm {cond = c; thbr = t; elbro = None; loc = Loc.make_location $loc} }
  | IF LPAREN c = comma_expr RPAREN t = statement ELSE e = statement      // phase 4: comma
      { IfThenElseStm {cond = c; thbr = t; elbro = Some e; loc = Loc.make_location $loc} }
  | WHILE LPAREN c = comma_expr RPAREN b = statement         // phase 4: comma
      { WhileStm {cond = c; body = b; loc = Loc.make_location $loc} }
  | FOR LPAREN i = option(for_init) SEMICOLON c = option(comma_expr) SEMICOLON
        u = option(comma_expr) RPAREN b = statement           // phase 4: comma
      { ForStm {init = i; cond = c; update = u; body = b; loc = Loc.make_location $loc} }
  | BREAK SEMICOLON
      { BreakStm {loc = Loc.make_location $loc} }
  | CONTINUE SEMICOLON
      { ContinueStm {loc = Loc.make_location $loc} }
  | LBRACE stms = list(statement) RBRACE
      { CompoundStm {stms; loc = Loc.make_location $loc} }
  | RETURN e = option(comma_expr) SEMICOLON                  // phase 4: comma
      { ReturnStm {ret = e; loc = Loc.make_location $loc} }

// Declarations: the comma separates declarations, so initializers are
// plain expressions (expr), never comma expressions.
decl_block:
  | VAR ds = separated_nonempty_list(COMMA, single_decl)
      { DeclBlock {declarations = ds; loc = Loc.make_location $loc} }

single_decl:
  | name = ident tp = option(preceded(COLON, typ)) ASSIGN body = expr
      { Declaration {name; tp; body; loc = Loc.make_location $loc} }

typ:
  | INT  { Int  {loc = Loc.make_location $loc} }
  | BOOL { Bool {loc = Loc.make_location $loc} }

for_init:
  | e = comma_expr  { FIExpr e }                              // phase 4: comma
  | db = decl_block { FIDecl db }

// ---------------------------------------------------------------------------
// Expressions
// comma_expr: full expression, may contain top-level commas.
// expr:       expression without top-level commas; used where the comma is a
//             separator (call arguments, declaration initializers) and for the
//             operands of all other operators. Parentheses re-allow commas.
// ---------------------------------------------------------------------------

comma_expr:                                                   // phase 4: comma
  | e = expr { e }
  | l = comma_expr COMMA r = expr
      { CommaExpr {left = l; right = r; loc = Loc.make_location $loc} }

expr:
  | i = INT_LIT                 { Integer {int = i; loc = Loc.make_location $loc} }
  | TRUE                        { Boolean {bool = true; loc = Loc.make_location $loc} }
  | FALSE                       { Boolean {bool = false; loc = Loc.make_location $loc} }
  | lv = lval                   { Lval lv }
  | LPAREN e = comma_expr RPAREN { e }                        // phase 4: comma
  | l = expr op = binop r = expr
      { BinOp {left = l; op; right = r; loc = Loc.make_location $loc} }
  | MINUS e = expr %prec UNARY
      { UnOp {op = Neg {loc = Loc.make_location ($startpos($1), $endpos($1))};
              operand = e; loc = Loc.make_location $loc} }
  | LNOT e = expr %prec UNARY
      { UnOp {op = Lnot {loc = Loc.make_location ($startpos($1), $endpos($1))};
              operand = e; loc = Loc.make_location $loc} }
  | lv = lval ASSIGN e = expr
      { Assignment {lvl = lv; rhs = e; loc = Loc.make_location $loc} }
  | f = ident LPAREN args = separated_list(COMMA, expr) RPAREN
      { Call {fname = f; args; loc = Loc.make_location $loc} }

%inline binop:
  | PLUS  { Plus  {loc = Loc.make_location $loc} }
  | MINUS { Minus {loc = Loc.make_location $loc} }
  | MUL   { Mul   {loc = Loc.make_location $loc} }
  | DIV   { Div   {loc = Loc.make_location $loc} }
  | REM   { Rem   {loc = Loc.make_location $loc} }
  | LT    { Lt    {loc = Loc.make_location $loc} }
  | LE    { Le    {loc = Loc.make_location $loc} }
  | GT    { Gt    {loc = Loc.make_location $loc} }
  | GE    { Ge    {loc = Loc.make_location $loc} }
  | EQ    { Eq    {loc = Loc.make_location $loc} }
  | NEQ   { NEq   {loc = Loc.make_location $loc} }
  | LAND  { Land  {loc = Loc.make_location $loc} }
  | LOR   { Lor   {loc = Loc.make_location $loc} }

lval:
  | id = ident { Var id }

ident:
  | name = IDENT { Ident {name; loc = Loc.make_location $loc} }