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

type pat = PVar | PTuple of pat list | PCon of conid * pat 

type loc = Lexing.position * Lexing.position

type 'a exp = 
  | Var of var
  | Abs of var * 'a 
  | Lam of pat * 'a 
  | App of 'a * 'a list 
  | Record of (field * 'a) list 
  | Proj of 'a * field
  | Tuple of 'a list 
  | Con of conid * 'a 
  | Case of 'a * (pat * 'a) list 
  | Annot of 'a * 'a 
  | Forall of 'a 
  | Lolli of 'a * 'a 
  | Tensor of 'a list 
  | With of (field * 'a) list 
  | Sum of (conid * 'a) list 
  | Mu of 'a 

let map f (e : 'a exp) = 
  match e with
  | Var x        -> Var x
  | Abs (x,e)    -> Abs(x, f e)
  | Lam (p,e)    -> Lam(p, f e)
  | App (e, es)  -> App(f e, List.map f es)
  | Record nes   -> Record (List.map (fun (n, e) -> (n, f e)) nes)
  | Proj (e, n)  -> Proj (f e, n)
  | Tuple es     -> Tuple (List.map f es)
  | Con (c,e)    -> Con(c, f e)
  | Case (e,pes) -> Case(f e, List.map (fun (p, e) -> (p, f e)) pes)
  | Annot (e,e') -> Annot(f e, f e')
  | Forall e     -> Forall (f e)
  | Lolli (e,e') -> Lolli(f e, f e')
  | Tensor es    -> Tensor (List.map f es)
  | With nes     -> With (List.map (fun (n, e) -> (n, f e)) nes)
  | Sum ces      -> With (List.map (fun (c, e) -> (c, f e)) ces)
  | Mu e         -> Mu (f e)

let join (m : 'a monoid) (e : 'a exp) = 
  let (zero, (+)) = m.unit, m.join in 
  let reduce es = List.fold_right (+) es zero in 
  match e with
  | Var x -> zero 
  | Abs (x, e) -> e
  | Lam (p,e) -> e
  | App (e,es) -> e + reduce es
  | Record nes -> reduce (List.map snd nes)
  | Proj (e,n) -> e
  | Tuple es -> reduce es
  | Con (c,e) -> e
  | Case (e,pes) -> e + reduce (List.map snd pes)
  | Annot (e,e') -> e + e'
  | Forall e -> e
  | Lolli (e,e') -> e + e'
  | Tensor es -> reduce es
  | With nes -> reduce (List.map snd nes)
  | Sum ces -> reduce (List.map snd ces)
  | Mu e -> e

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
