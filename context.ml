open Ast
open Util

type time = Later | Now | Always 
type count = One | Zero | Aff
type usage = Int | Lin of count * time
type sort = Univ | Exist  

type tp = Ast.t

type hide = Linear | Transient
            
(* TODO: add markers to model !A *)    
type hyp = 
  | Tm of tp * usage
  | Tp of sort * tp option 
  | Mark 
  | Hide of hide
(*  | Def of int * tp (* Arity plus definition (with arity binders) *) *)

type ctx = (var * hyp * loc) list 

type error = 
  | Unbound of var 
  | Reuse of var
  | Usage of var
  | Hidden of hide * var
  | Unused of var * loc 
  

type state = {ctx : ctx; loc: loc; gensym : int}

type 'a t = Cmd of (state -> ('a * state, error * loc) result)

let return x = Cmd(fun s -> Ok(x, s))
let (>>=) (Cmd c) f = 
  Cmd(fun s -> 
        match c s with
	| Error err -> Error err
	| Ok(v, s) -> let Cmd c = f v in c s)
let map f (Cmd c) = 
  Cmd(fun s -> 
      match c s with
      | Error err -> Error err
      | Ok(v, s) -> Ok(f v, s))

module M = struct type 'a s =  'a t
                  type 'a t = 'a s 
                  let map = map 
                  let return = return 
                  let (>>=) = (>>=) 
           end 

let seq_ast e = 
  let module S = Ast.Seq(Util.Idiom(M)) in 
  S.seq e 

let error err = Cmd(fun s -> Error (err, s.loc))

let get_loc = Cmd(fun s -> Ok(s.loc, s))
let set_loc loc = Cmd(fun s -> Ok((), {s with loc = loc}))
let get_ctx = Cmd(fun s -> Ok(s.ctx, s))
let set_ctx ctx = Cmd(fun s -> Ok((), {s with ctx = ctx}))


let gensym name = Cmd(fun state -> 
                      Ok(Printf.sprintf "%s_%d" name state.gensym, 
                             {state with gensym = state.gensym + 1}))


let visibility x usage hide = 
  match hide, usage with 
  | Linear, Int               -> Ok Int
  | Transient, Int            -> Ok Int 
  | Linear, Lin(_, _)         -> Error (Hidden(Linear, x))
  | Transient, Lin(_, Later)  -> Error (Hidden(Transient, x))
  | Transient, Lin(_, Now)    -> Error (Hidden(Transient, x))
  | Transient, Lin(u, Always) -> Ok (Lin(u, Always))



let rec lookup x = function
  | [] -> Error (Unbound x)
  | (y, Tm(tp, Lin(One, t)), loc) :: ctx when y = x  -> Ok(Tm(tp, Lin(One, t)), (y, Tm(tp, Lin(Zero, t)), loc) :: ctx)
  | (y, Tm(tp, Lin(Aff, t)), loc) :: ctx when y = x  -> Ok(Tm(tp, Lin(Aff, t)), (y, Tm(tp, Lin(Zero, t)), loc) :: ctx)
  | (y, Tm(tp, Lin(Zero, t)), loc) :: ctx when y = x -> Error (Reuse x)
  | (y, h, loc) :: ctx when y = x -> Ok(h, (y, h, loc) :: ctx)
  | (y, h, loc) :: ctx -> (match lookup x ctx with
                           | Error e -> Error e
		           | Ok(r, ctx) -> Ok(r, (y, h, loc) :: ctx))

let lookup x = get_ctx >>= fun ctx ->
               match lookup x ctx with
	       | Error e -> error e
	       | Ok (v, ctx) -> set_ctx ctx >>= fun () ->
		                    return v

let with_hyp (x, h, loc) cmd = 
  let rec pop = function
    | [] -> assert false 
    | (y, h, loc) :: ctx when x = y -> ((y, h, loc), ctx)
    | (y, h, loc) :: ctx -> pop ctx 
  in 
  let check = function
    | Tm(_, Lin(One, _)) -> false 
    | _                   -> true 
  in 
  get_ctx >>= fun ctx -> 
  set_ctx ((x, h, loc) :: ctx) >>= fun () ->
  cmd >>= fun v -> 
  get_ctx >>= fun ctx -> 
  let ((y, h, loc), ctx) = pop ctx in 
  if check h then 
    set_ctx ctx >>= fun () -> 
    return v
  else
    error (Unused(y, loc))

let rec split_context x = function
  | [] -> assert false
  | (y, h, loc) :: ctx when x = y -> ([y, h, loc], ctx)
  | (y, h, loc) :: ctx -> let (front, back) = split_context x ctx in 
                          ((y, h, loc) :: front, back)  
          
let before x cmd = 
  get_ctx >>= fun ctx -> 
  let (front, back) = split_context x ctx in
  set_ctx back >>= fun () ->
  cmd >>= fun v -> 
  get_ctx >>= fun back' -> 
  set_ctx (front @ back') >>= fun () -> 
  return v 

let out e = 
  match Ast.out e with 
  | Abs(x, e) -> gensym x >>= fun y -> 
                 return (Abs(y, Ast.rename x y e))
  | e'        -> return e'

let rec subst e x ebody = 
  out ebody >>= function 
  | Var z when x = z -> return e
  | t -> seq_ast (Ast.map (subst e x) t) >>= fun t' -> 
         return (into (loc ebody) t')

         
let rec seq = function
  | [] -> return []
  | c :: cs -> c >>= fun v -> 
               seq cs >>= fun vs -> 
               return (v :: vs)

let rec reset_usage (newctx :ctx) (oldctx :ctx)= 
  match (newctx, oldctx) with
  | ([],[]) -> []
  | (x1, Tm(tp1, u1), l1) :: ctx1, (x2, Tm(tp2, u2), l2) :: ctx2 -> 
    if x1 = x2 then 
      (x1, Tm(tp1, u2), l1) :: reset_usage ctx1 ctx2 
    else
      assert false 
  | (x1, h, l1) :: ctx1,             (((_, Tm(_, _), _) :: _) as ctx2) -> (x1, h, l1) :: reset_usage ctx1 ctx2 
  | ((_, Tm(_, _), _) :: _) as ctx1, (_, _, _) :: ctx2                 -> reset_usage ctx1 ctx2
  | ((_, Tm(_, _), _) :: _),         []                                -> assert false 
  | [],                              ((_, Tm(_, _), _) :: _)           -> assert false 
  | ((x1, h, l) :: ctx1),            ctx2                              -> (x1, h, l) :: reset_usage ctx1 ctx2
  | ctx1,                            (_, _, _) :: ctx2                 -> reset_usage ctx1 ctx2


let join_usage_lin u u' = 
  match u, u' with
  | Aff,  One  -> Some One
  | One,  Aff  -> Some One
  | Aff,  Zero -> Some Zero
  | Zero, Aff  -> Some Zero
  | _          -> if u = u' then Some u else None

let join_usage u u' = 
  match u, u' with
  | Int, Int               -> Some Int
  | Lin(u, t), Lin(u', t') -> (match join_usage_lin u u' with
                               | Some u'' -> if t = t' then Some(Lin(u'', t)) else assert false
                               | None -> None)
  | _                      -> None

  
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

