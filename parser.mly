%{
open Ast 

let make e = into (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) e

let forall x tp = make (Forall (make (Abs(x, tp))))

let rec abs xs e = 
  match xs with
  | e -> e 
  | x :: xs -> make (Abs(x, abs xs e))

let rec func (p, xs) e = 
  make (Lam(p, abs xs e))
%}

%token TYPE
%token FORALL
%token OF
%token DOT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token COMMA
%token SEMI
%token BANG
%token FUN
%token TO
%token PLUS
%token MINUS
%token LT
%token LEQ
%token GEQ
%token GT
%token TENSOR
%token AND
%token ANDAND
%token OR
%token LET
%token COLON
%token EQUAL
%token IN
%token FIX
%token IF
%token THEN
%token ELSE
%token VAL
%token REC
%token MATCH
%token WITH
%token BAR
%token UNDERSCORE
%token<int> NUM
%token RUN
%token BOOLTYPE
%token INTTYPE
%token LOLLI
%token UNITTYPE
%token I
%token END
%token<string> IDENT
%token<string> CONID
%token<string> STRING
%token EOF


%start tp_start
%type<Ast.t> tp_start
  
%%

tp_atom :
| IDENT           { make (Var $1) }
| LBRACE record_fields RBRACE         { make (With $2) }
| LBRACK sum_fields    RBRACK         { make (Sum $2)  }
| LPAREN tp RPAREN                    { make (out $2)  } 
| LPAREN tp_atom tensor_fields RPAREN { make (Tensor ($2 :: $3)) }
| FIX IDENT DOT LBRACK sum_fields RBRACK { make (Mu(abs $2 (make (Sum $5)))) }
;

tp_app :
| tp_atom         { $1 }
| BANG tp_atom         { make (Bang $2) }
| tp_app tp_atom  { make (App($1, $2)) }
;

tp : 
|  tp_app          { $1 }
|  tp_app LOLLI tp { make (Lolli($1, $3)) }
|  FORALL IDENT DOT tp { forall $2 $4 }
;


record_fields :
| { [] }
| IDENT COLON tp { [$1, $3] }
| IDENT COLON tp SEMI record_fields  { ($1, $3) :: $5 }
;

sum_fields :
| { [] }
| CONID COLON tp                    { [$1, $3] }
| CONID COLON tp BAR record_fields  { ($1, $3) :: $5 }
;

tensor_fields :
| TENSOR tp                { [$2] }
| TENSOR tp tensor_fields  { $2 :: $3}
;

tp_start :
  tp EOF { $1 }
;
