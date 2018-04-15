{
	open Hiq_parse
	open Lexing
}

let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let identifier = ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z' '0'-'9' '_'])* 

rule lexer = parse
| eol { lexer lexbuf } 
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| "/*" { comment lexbuf }
| "if" { IF }
| "else" { ELSE }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMI }
| "cbit" { CBIT }
| "qubit" { QUBIT }
| "H" { H }
| "CNOT" { CNOT }
| "measure" { MEASURE }
| eof { EOF }

and comment = parse
| "*/" { lexer lexbuf }
| eof { raise (Failure "missing comment terminator") }
| _ { comment lexbuf }