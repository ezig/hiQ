%{
open Hiq_ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

%type <Hiq_ast.program> program
%type <Hiq_ast.stmt> stmt
%type <Hiq_ast.exp> exp

%token <int> INT
%nonassoc LOWER_THAN_ELSE
%token QUBIT CBIT IF ELSE WHILE LPAREN RPAREN LBRACE RBRACE COMMA
%token H X CNOT MEASURE
%token SEMI
%token EOF

%%

program:
  CBIT INT QUBIT INT stmt EOF { {cbits = $2; qubits = $4; prog = $5} }

stmt :
| exp { Exp ($1) }
| stmt SEMI stmt { Seq ($1, $3) }
| IF LPAREN exp RPAREN LBRACE stmt RBRACE { If ($3, $6, None) } %prec LOWER_THAN_ELSE 
| IF LPAREN exp RPAREN LBRACE stmt RBRACE ELSE LBRACE stmt RBRACE { If ($3, $6, Some $10) } 

exp:
| MEASURE LPAREN INT COMMA INT RPAREN { Measure ($3, $5) }
| H LPAREN INT RPAREN { UGate (H, $3) }
| X LPAREN INT RPAREN { UGate (X, $3) }
| CNOT LPAREN INT COMMA INT RPAREN { BinGate (CNOT, $3, $5) }
