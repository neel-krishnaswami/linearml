{
  open Parser

  let stringfold f init s = 
    let n = String.length s in
    let r = ref init in
    for i = 0 to n-1 do r := f s.[i] (!r) done;
    !r

  let count_newlines s =
    stringfold (fun c n -> if c = '\n' then n+1 else n) 0 s 

  let repeat n thunk = for i = 0 to n-1 do thunk() done
}
let comment = "//" [^'\n']* "\n"
let digit  = ['0'-'9']
let int = '-'? digit+ 
let lident = ['a' - 'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'' '?']*
let uident = ['A' - 'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'' '?']*
let whitespace = ['\t' ' ']+
let new_line = '\n' | '\r' | '\r' '\n'
let string_literal = ([^'\\' '\"' '\n'] | "\\n" | "\\t" | "\\\\" |"\\\"" )* 

rule token = parse
  | "type"                  {TYPE}
  | "forall"                {FORALL}
  | "∀"                     {FORALL}
  | "of"                    {OF}
  | "."                     {DOT}
  | "("       		    {LPAREN}
  | ")"       		    {RPAREN}
  | "{"                     {LBRACE}
  | "}"                     {RBRACE}
  | "["                     {LBRACK}
  | "}"                     {RBRACK}
  | ","       		    {COMMA}
  | ";"                     {SEMI}
  | "!"       		    {BANG}
  | "fun"     		    {FUN}
  | "λ"     		    {FUN}
  | "->"      		    {TO}
  | "→"      		    {TO}
  | "+"       		    {PLUS}
  | "-"       		    {MINUS}
  | "<"                     {LT}
  | "<="                    {LEQ}
  | "≤"                     {LEQ}
  | ">="                    {GEQ}
  | "≥"                     {GEQ}
  | ">"                     {GT}
  | "**"                    {TENSOR}
  | "⊗"       		    {TENSOR}
  | "&"                     {AND}
  | "×"                     {AND}
  | "&&"      		    {ANDAND}
  | "||"      		    {OR}
  | "let"     		    {LET}
  | ":"       		    {COLON}
  | "="       		    {EQUAL}
  | "in"      		    {IN}
  | "fix"     		    {FIX}
  | "μ"                     {FIX}
  | "if"      		    {IF}
  | "then"    		    {THEN}
  | "else"    		    {ELSE}
  | "val"                   {VAL}
  | "rec"                   {REC}
  | "match"                 {MATCH}
  | "with"                  {WITH}
  | "|"                     {BAR}
  | "_"                     {UNDERSCORE}
  | int as n                {NUM(int_of_string n)}
  | '\"' (string_literal as s) '\"' {repeat (count_newlines s) (fun () -> Lexing.new_line lexbuf); STRING s}
  | "run"                   {RUN}
  | "bool"                  {BOOLTYPE}
  | "int"                   {INTTYPE}
  | "-o"                    {LOLLI}
  | "⊸"                    {LOLLI}
  | "unit"                  {UNITTYPE}
  | "⊤"                     {UNITTYPE}
  | "I"                     {I}
  | "end"                   {END}
  | lident as s             {IDENT s}
  | uident as s             {CONID s}
  | comment                 {Lexing.new_line lexbuf; token lexbuf}
  | whitespace              {token lexbuf}
  | new_line                {Lexing.new_line lexbuf; token lexbuf}
  | eof                     {EOF}

