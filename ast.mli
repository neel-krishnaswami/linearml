type var = string
type conid = string
type field = string
type loc = Lexing.position * Lexing.position
                               
module V : (Set.S with type elt = var)

type pat =
    PVar
  | PTuple of pat list
  | PCon of conid * pat
  | PF of pat
  | PAlways of pat
  | PEvent of pat

type 'a exp =
    Var of var
  | Abs of var * 'a
  | Lam of pat * 'a
  | App of 'a * 'a
  | Record of (field * 'a) list
  | Proj of 'a * field
  | Tuple of 'a list
  | Con of conid
  | Case of 'a * (pat * 'a) list
  | Annot of 'a * 'a
  | Num of int
  | Select of ('a * 'a) list
  | Yield of 'a
  | Forall of 'a * 'a 
  | Exists of 'a * 'a
  | Lolli of 'a * 'a
  | Arrow of 'a * 'a
  | Tensor of 'a list
  | With of (field * 'a) list
  | Sum of (conid * 'a) list
  | Mu of 'a * 'a 
  | F of 'a
  | G of 'a
  | Always of 'a
  | Event of 'a
  | Type
  | Linear

val map : ('a -> 'b) -> 'a exp -> 'b exp

module Seq(M : Util.IDIOM) : sig
    val seq : 'a M.t exp -> 'a exp M.t
  end


type t = In of loc * V.t * t exp

val loc : t -> loc
val fvs : t -> V.t
val out : t -> t exp

val into : loc -> t exp -> t

val rename : var -> var -> t -> t

module Constr : sig
    type con
    val var : var -> con
    val abs : var * con -> con
    val lam : pat * con -> con
    val app : con * con -> con
    val record : (field * con) list -> con
    val proj : con * field -> con
    val tuple : con list -> con
    val con : conid -> con
    val case : con * (pat * con) list -> con
    val annot : con * con -> con
    val num : int -> con
    val yield : con -> con
    val select : (con * con) list -> con
    val forall : con -> con
    val exists : con -> con
    val lolli : con * con -> con
    val tensor : con list -> con
    val witht : (field * con) list -> con
    val sum : (conid * con) list -> con
    val mu : con * con -> con
    val f : con -> con
    val g : con -> con
    val always : con -> con
    val event : con -> con
    val type' : con
    val linear : con
    val ( @ ) : con -> loc -> con
    val get : con -> loc -> t
  end

