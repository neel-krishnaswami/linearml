type 'a monoid = {unit : 'a ; join : 'a -> 'a -> 'a}

type var = string
type conid = string (* Leading uppercase *)
type field = string

module V = Set.Make(struct type t = var let compare = compare end)
let var = {unit = V.empty ; join = V.union}
let rec freshen x vs = 
  if V.mem x vs then 
    freshen (x ^ "'") vs 
  else
    x

type pat = PVar | PTuple of pat list | PCon of conid * pat | PF of pat | PAlways of pat | PEvent of pat 

type loc = Lexing.position * Lexing.position

type 'a exp = 
  | Var of var
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
  | Forall of 'a 
  | Exists of 'a 
  | Lolli of 'a * 'a 
  | Tensor of 'a list 
  | With of (field * 'a) list 
  | Sum of (conid * 'a) list
  | Mu of 'a 
  | F of 'a 
  | G of 'a 
  | Always of 'a 
  | Event of 'a

let map f (e : 'a exp) = 
  match e with
  | Var x        -> Var x
  | Abs (x,e)    -> Abs(x, f e)
  | Lam (p,e)    -> Lam(p, f e)
  | App (e, e')  -> App(f e, f e')
  | Record nes   -> Record (List.map (fun (n, e) -> (n, f e)) nes)
  | Proj (e, n)  -> Proj (f e, n)
  | Tuple es     -> Tuple (List.map f es)
  | Con c        -> Con c
  | Case (e,pes) -> Case(f e, List.map (fun (p, e) -> (p, f e)) pes)
  | Annot (e,e') -> Annot(f e, f e')
  | Num n        -> Num n 
  | Select ets   -> Select (List.map (fun (e, t) -> (f e, f t)) ets)
  | Yield e      -> Yield (f e)
  | Forall e     -> Forall (f e)
  | Exists e     -> Exists (f e)
  | Lolli (e,e') -> Lolli(f e, f e')
  | Tensor es    -> Tensor (List.map f es)
  | With nes     -> With (List.map (fun (n, e) -> (n, f e)) nes)
  | Sum ces      -> With (List.map (fun (c, e) -> (c, f e)) ces)
  | Mu e         -> Mu (f e)
  | F e          -> F (f e)
  | G e          -> G (f e)
  | Always e     -> Always (f e)
  | Event e      -> Event (f e)

let join (m : 'a monoid) (e : 'a exp) = 
  let (zero, (+)) = m.unit, m.join in 
  let reduce es = List.fold_right (+) es zero in 
  match e with
  | Var x -> zero 
  | Abs (x, e) -> e
  | Lam (p,e) -> e
  | App (e,e') -> e + e'
  | Record nes -> reduce (List.map snd nes)
  | Proj (e,n) -> e
  | Tuple es -> reduce es
  | Con c -> zero 
  | Case (e,pes) -> e + reduce (List.map snd pes)
  | Annot (e,e') -> e + e'
  | Num n        -> zero
  | Select ets -> reduce (List.map (fun (e,t) -> e + t) ets)
  | Yield e -> e 
  | Forall e -> e
  | Exists e -> e
  | Lolli (e,e') -> e + e'
  | Tensor es -> reduce es
  | With nes -> reduce (List.map snd nes)
  | Sum ces -> reduce (List.map snd ces)
  | Mu e -> e
  | F e -> e 
  | G e -> e 
  | Always e -> e 
  | Event e -> e 

type t = In of loc * V.t * t exp 

let loc (In(loc, _, _)) = loc
let fvs (In(_, vs, _)) = vs
let out (In(_, _, body)) = body 

let into loc body = 
  let vs = 
    match map fvs body with
    | Var x      -> V.singleton x 
    | Abs(x, vs) -> V.remove x vs
    | evs        -> join var evs
  in 
  In(loc, vs, body)

let rec rename x y e = 
  into (loc e) 
       (match out e with
       | Var z when x = z -> Var y 
       | Abs(z, e') when x = z -> Abs(z, e')
       | body -> map (rename x y) body)

let rec subst e x ebody = 
  match out ebody with
  | Var z when x = z -> e
  | Abs(z, _) when x = z -> ebody
  | Abs(z, e') when V.mem z (fvs e) -> let z' = freshen z (fvs e) in
				       into (loc ebody) (Abs(z', subst e x (rename z z' e')))
  | _ -> into (loc ebody) (map (subst e x) (out ebody))

module type CONSTR = sig 
  type con

  val var : var -> con
  val abs : var * con  -> con
  val lam : pat * con  -> con
  val app : con * con  -> con
  val record : (field * con) list  -> con
  val proj : con * field -> con
  val tuple : con list  -> con
  val con : conid  -> con
  val case : con * (pat * con) list  -> con
  val annot : con * con -> con
  val num : int  -> con
  val yield : con -> con
  val select : (con * con) list -> con 
  val forall : con  -> con
  val exists : con  -> con
  val lolli : con * con  -> con
  val tensor : con list  -> con
  val witht : (field * con) list  -> con
  val sum : (conid * con) list -> con
  val mu : con  -> con
  val f : con  -> con
  val g : con  -> con
  val always : con  -> con
  val event : con  -> con
                     
  val (@) : con -> loc -> con

  val get : con -> loc -> t 
end

module Constr : CONSTR = struct
  type con = loc -> t 

  let var : var -> con = 
    fun x loc -> into loc (Var x)                    

  let abs : var * con  -> con =
    fun (x, c) loc -> 
    into loc (Abs(x, c loc))

  let lam : pat * con  -> con =
    fun (p, c) loc -> 
    into loc (Lam(p, c loc))

  let app : con * con  -> con =
    fun (f, e) loc -> into loc (App(f loc, e loc))

  let record : (field * con) list  -> con =
    fun fcs loc -> let fts = List.map (fun (f, c) -> (f, c loc)) fcs in 
                   into loc (Record fts)

  let proj : con * field -> con =
    fun (c, f) loc -> into loc (Proj(c loc, f))

  let tuple : con list  -> con =
    fun cs loc -> into loc (Tuple (List.map ((|>) loc) cs))

  let con : conid  -> con =
    fun con loc -> into loc (Con con)

  let case : con * (pat * con) list  -> con =
    fun (c, pcs) loc ->
      into loc (Case(c loc, List.map (fun (p, c) -> (p, c loc)) pcs))

  let annot : con * con -> con =
    fun (f, e) loc -> into loc (Annot(f loc, e loc))

  let num : int  -> con =
    fun x loc -> into loc (Num x)                    

  let select : (con * con) list -> con  = 
    fun ets loc -> into loc (Select (List.map (fun (e, t) -> (e loc, t loc)) ets))


  let yield : con  -> con =
    fun e loc -> into loc (Yield (e loc))
                      
  let forall : con  -> con =
    fun e loc -> into loc (Forall(e loc))

  let exists : con  -> con =
    fun e loc -> into loc (Exists(e loc))
                      
  let lolli : con * con  -> con =
    fun (f, e) loc -> into loc (Lolli(f loc, e loc))

  let tensor : con list  -> con =
    fun cs loc -> into loc (Tensor (List.map ((|>) loc) cs))

  let witht : (field * con) list  -> con =
    fun fcs loc -> let fts = List.map (fun (f, c) -> (f, c loc)) fcs in 
                   into loc (With fts)

  let sum : (conid * con) list -> con =
    fun ncs loc -> let nts = List.map (fun (n, c) -> (n, c loc)) ncs in 
                   into loc (Sum nts)

  let mu : con  -> con =
    fun e loc -> into loc (Mu(e loc))

 
  let f : con  -> con =
    fun e loc -> into loc (F(e loc))

  let g : con  -> con =
    fun e loc -> into loc (G(e loc))

  let always : con  -> con =
    fun e loc -> into loc (Always(e loc))

  let event : con  -> con =
    fun e loc -> into loc (Event(e loc))

                      
  let (@) : con -> loc -> con =
    fun c loc _ -> c loc

  let get : con -> loc -> t  =
    fun c l -> c l 

end

  
