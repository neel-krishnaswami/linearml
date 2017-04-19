type time = Later | Now | Always
type count = One | Zero | Aff
type usage = Int | Lin of count * time
type sort = Univ | Exist
type tp = Ast.t
type kind = Ast.t
type hide = Linear | Transient
type hyp =
    Tm of tp * usage
  | Tp of sort * kind * tp option
  | Mark
  | Hide of hide
type ctx = (Ast.var * (hyp * Ast.loc)) list
type error =
    Unbound of Ast.var
  | Reuse of Ast.var
  | Usage of Ast.var
  | Hidden of hide * Ast.var
  | Unused of Ast.var * Ast.loc
  | NotEvar of Ast.var
  | IllSorted of Ast.var * string
  | NotEqual of tp * tp * string
  | Synth_mismatch of tp * string
  | Check_mismatch of tp * string

                         
type 'a t

val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t

val seq_ast : 'a t Ast.exp -> 'a Ast.exp t
val error : error -> 'a t
val get_loc : Ast.loc t
val set_loc : Ast.loc -> unit t

val gensym : string -> string t

val lookup : Ast.var -> hyp t
val with_hyp : Ast.var * hyp -> 'a t -> 'a t

val before : Ast.var -> 'a t -> 'a t
val into : Ast.t Ast.exp -> Ast.t t
val out : Ast.t -> Ast.t Ast.exp t
val subst : Ast.t -> Ast.var -> Ast.t -> Ast.t t

module Par : (Util.SEQ with type 'a t = 'a t)
module Seq : (Util.SEQ with type 'a t = 'a t)

val evar : kind -> Ast.var t
val inst : Ast.var -> tp -> unit t

val run : ctx -> Ast.loc -> 'a t -> ('a, error * Ast.loc) result 
