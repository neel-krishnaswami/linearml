open Ast

type usage = Infty | One | Zero | Aff
type sort = Univ | Exist  


(* TODO: add markers to model !A *)    
type hyp = 
  | Tm of tp * usage
  | Tp of sort * tp option 
  | Mark 
  | Def of int * tp (* Arity plus definition (with arity binders) *)

type ctx = (var * hyp) list 

type error = 
  | Unbound of var 
  | Reuse of var
  | Usage of var
  
type ('a, 'b) result = Error of 'b | Result of 'a

type state = {ctx : ctx; loc: loc}

type 'a t = Cmd of (state -> ('a * state, error * loc) result)

let return x = Cmd(fun s -> Result(x, s))
let (>>=) (Cmd c) f = 
  Cmd(fun s -> 
        match c s with
	| Error err -> Error err
	| Result(v, s) -> let Cmd c = f v in c s)

let error err = Cmd(fun s -> Error (err, s.loc))

let get_loc = Cmd(fun s -> Result(s.loc, s))
let set_loc loc = Cmd(fun s -> Result((), {s with loc = loc}))
let get_ctx = Cmd(fun s -> Result(s.ctx, s))
let set_ctx ctx = Cmd(fun s -> Result((), {s with ctx = ctx}))
  

let rec lookup x = function
  | [] -> Error (Unbound x)
  | (y, Tm(tp, One)) :: ctx when y = x  -> Result(Tm(tp, One), (y, Tm(tp, Zero)) :: ctx)
  | (y, Tm(tp, Aff)) :: ctx when y = x  -> Result(Tm(tp, Aff), (y, Tm(tp, Zero)) :: ctx)
  | (y, Tm(tp, Zero)) :: ctx when y = x -> Error (Reuse x)
  | (y, h) :: ctx when y = x -> Result(h, (y, h) :: ctx)
  | (y, h) :: ctx -> (match lookup x ctx with
                      | Error e -> Error e
		      | Result(r, ctx) -> Result(r, (y, h) :: ctx))

let lookup x = get_ctx >>= fun ctx ->
               match lookup x ctx with
	       | Error e -> error e
	       | Result (v, ctx) -> set_ctx ctx >>= fun () ->
		                    return v

let with_hyp (x, h) cmd = 
  let rec pop = function
    | [] -> assert false 
    | (y, h) :: ctx when x = y -> ctx
    | (y, h) :: ctx -> pop ctx 
  in 
  get_ctx >>= fun ctx -> 
  set_ctx ((x, h) :: ctx) >>= fun () ->
  cmd >>= fun v -> 
  get_ctx >>= fun ctx -> 
  set_ctx (pop ctx) >>= fun () -> 
  return v

let fresh x = 
  get_ctx >>= fun ctx -> 
  let vars = V.of_list (List.map fst ctx) in
  return (Ast.freshen x vars)
  
let unabs t = 
  match out t with 
  | Abs(x, t) -> fresh x >>= fun y -> 
                 return (y, rename x y t)
  | _ -> assert false

let rec seq = function
  | [] -> return []
  | c :: cs -> c >>= fun v -> 
               seq cs >>= fun vs -> 
               return (v :: vs)

let rec reset_usage (newctx :ctx) (oldctx :ctx)= 
  match (newctx, oldctx) with
  | ([],[]) -> []
  | (x1, Tm(tp1, u1)) :: ctx1,    (x2, Tm(tp2, u2)) :: ctx2 -> 
    if x1 = x2 then 
      (x1, Tm(tp1, u2)) :: reset_usage ctx1 ctx2 
    else
      assert false 
  | (x1, h) :: ctx1,              (((_, Tm(_, _)) :: _) as ctx2) -> (x1, h) :: reset_usage ctx1 ctx2 
  | ((_, Tm(_, _)) :: _) as ctx1, (_, _) :: ctx2                 -> reset_usage ctx1 ctx2
  | ((_, Tm(_, _)) :: _),         []                             -> assert false 
  | [],                           ((_, Tm(_, _)) :: _)           -> assert false 
  | ((x1, h) :: ctx1),            ctx2                           -> (x1, h) :: reset_usage ctx1 ctx2
  | ctx1,                         (_, _) :: ctx2                 -> reset_usage ctx1 ctx2


let usage_ok u u' =
  match u, u' with 
  | One, Aff | Aff, One -> true
  | Zero, Aff | Aff, Zero -> true
  | _ -> u = u'
  
  
let rec compatible ctx1 ctx2 = 
  match ctx1, ctx2 with
  | ([],[]) -> return ()
  | (x1, Tm(tp1, u1)) :: ctx1,    (x2, Tm(tp2, u2)) :: ctx2 -> 
    if x1 = x2 then 
      if usage_ok u1 u2 then 
	compatible ctx1 ctx2 
      else
	error (Usage x1)
    else
      assert false 
  | (_, _) :: ctx1,               (((_, Tm(_, _)) :: _) as ctx2) -> compatible ctx1 ctx2 
  | ((_, Tm(_, _)) :: _) as ctx1, (_, _) :: ctx2                 -> compatible ctx1 ctx2
  | ((_, Tm(_, _)) :: _),         []                             -> assert false
  | [],                           ((_, Tm(_, _)) :: _)           -> assert false 
  | ((_, _) :: _),                ctx2                           -> compatible ctx1 ctx2
  | ctx1,                         (_, _) :: ctx2                 -> compatible ctx1 ctx2

let parallel c1 c2 = 
  get_ctx >>= fun old -> 
  c1 >>= fun v1 -> 
  get_ctx >>= fun ctx1 -> 
  set_ctx (reset_usage ctx1 old) >>= fun () -> 
  c2 >>= fun v2 -> 
  get_ctx >>= fun ctx2 -> 
  compatible ctx1 ctx2 >>= fun () -> 
  return (v1, v2)

let rec par = function
  | [] -> return []
  | [c] -> c >>= fun v -> return [v]
  | c :: cs -> parallel c (par cs) >>= fun (v, vs) ->
               return (v :: vs)
