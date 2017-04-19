open Ast
open Util

type time = Later | Now | Always 
type count = One | Zero | Aff
type usage = Int | Lin of count * time
type sort = Univ | Exist  

type tp = Ast.t
type kind = Ast.t 

type hide = Linear | Transient
            
(* TODO: add markers to model !A *)    
type hyp = 
  | Tm of tp * usage
  | Tp of sort * kind * tp option 
  | Mark 
  | Hide of hide
(*  | Def of int * tp (* Arity plus definition (with arity binders) *) *)

type ctx = (var * (hyp * loc)) list 

type error = 
  | Unbound of var 
  | Reuse of var
  | Usage of var
  | Hidden of hide * var
  | Unused of var * loc 
  | NotEvar of var
  | IllSorted of var * string 
  | NotEqual of tp * tp * string
  | Synth_mismatch of tp * string
  | Check_mismatch of tp * string

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
  let module S = Ast.Seq(Util.Monoidal(M)) in 
  S.seq e 

let error err = Cmd(fun s -> Error (err, s.loc))

let get_loc = Cmd(fun s -> Ok(s.loc, s))
let set_loc loc = Cmd(fun s -> Ok((), {s with loc = loc}))
let get_ctx = Cmd(fun s -> Ok(s.ctx, s))
let set_ctx ctx = Cmd(fun s -> Ok((), {s with ctx = ctx}))


let gensym name = Cmd(fun state -> 
                      Ok(Printf.sprintf "%s_%d" name state.gensym, 
                             {state with gensym = state.gensym + 1}))



let acquire usage hidden = 
  let open Result in 
  match usage with 
  | Int -> return(Int, Int)
  | Lin(Zero, _) -> Error (fun x -> Reuse x)
  | Lin(u, t) when List.mem Linear hidden -> Error(fun x -> Hidden(Linear, x))
  | Lin(u, Later) when List.mem Transient hidden -> Error(fun x -> Hidden(Transient, x))
  | Lin(u, Now)   when List.mem Transient hidden -> Error(fun x -> Hidden(Transient, x))
  | Lin(u, t) -> return (Lin(u, t), Lin(Zero, t))

let rec lookup hidden x ctx = 
  let open Result in 
  match ctx with 
  | []                                      -> Error (fun x -> Unbound x)
  | (y, (Tm(tp, u), loc)) :: ctx when y = x -> acquire u hidden >>= fun (u, u') -> 
                                               return (Tm(tp, u), (y, (Tm(tp, u'), loc)) :: ctx)
  | (y, (h, loc)) :: ctx when y = x         -> return (h, (y, (h, loc)) :: ctx)
  | (y, (Hide h, loc)) :: ctx               -> lookup (h :: hidden) x ctx >>= fun (r, ctx) -> 
		                               return (r, (y, (Hide h, loc)) :: ctx)
  | (y, h) :: ctx                           -> lookup hidden x ctx >>= fun (v, ctx) -> 
                                               return (v, (y, h) :: ctx)
                     
                                   


let lookup x = get_ctx >>= fun ctx ->
               match lookup [] x ctx with
	       | Error e -> error (e x)
	       | Ok (v, ctx) -> set_ctx ctx >>= fun () ->
		                return v
                                           
let with_hyp (x, h) cmd = 
  let rec pop = function
    | [] -> assert false 
    | (y, (h, loc)) :: ctx when x = y -> ((y, h, loc), ctx)
    | (y, (h, loc)) :: ctx -> pop ctx 
  in 
  let check = function
    | Tm(_, Lin(One, _)) -> false 
    | _                  -> true 
  in 
  get_loc >>= fun loc -> 
  get_ctx >>= fun ctx -> 
  set_ctx ((x, (h, loc)) :: ctx) >>= fun () ->
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
  | (y, h) :: ctx when x = y -> ([y, h], ctx)
  | (y, h) :: ctx -> let (front, back) = split_context x ctx in 
                         ((y, h) :: front, back)  
          
let before x cmd = 
  get_ctx >>= fun ctx -> 
  let (front, back) = split_context x ctx in
  set_ctx back >>= fun () ->
  cmd >>= fun v -> 
  get_ctx >>= fun back' -> 
  set_ctx (front @ back') >>= fun () -> 
  return v 

let into e = 
  get_loc >>= fun loc -> 
  return (Ast.into loc e)

let out e = 
  match Ast.out e with 
  | Abs(x, e) -> gensym x >>= fun y -> 
                 return (Abs(y, Ast.rename x y e))
  | e'        -> return e'

let rec subst e x ebody = 
  out ebody >>= function 
  | Var z when x = z -> return e
  | t -> seq_ast (Ast.map (subst e x) t) >>= fun t' -> 
         return (Ast.into (loc ebody) t')

let rec merge f err (oldctx : ctx) (newctx : ctx) = 
  match oldctx, newctx with 
  | [], newctx                                                      -> 
     return newctx
  | (x1, (h1, l1)) :: oldctx, (x2, (h2, l2)) :: newctx when x1 = x2 -> 
     (match f h1 h2 with 
      | Some h -> merge f err oldctx newctx >>= fun ctx -> 
                  return ((x2, (h, l2)) :: ctx)
      | None   -> error (err x1))
  | oldctx, (x, h) :: newctx when not (List.mem_assoc x oldctx)     -> 
     merge f err oldctx newctx >>= fun ctx -> 
     return ((x, h) :: ctx)
  | _, _ -> assert false 


let reset_usage (oldctx : ctx) (newctx : ctx) = 
  let reset (oldh : hyp) (newh : hyp) = 
    match oldh, newh with
    | Tm(_, u), Tm(tp, _) -> Some (Tm(tp, u))
    | _, _                -> Some newh 
  in 
  merge reset (fun x -> Usage x) oldctx newctx


let join_usage_lin u u' = 
  match u, u' with
  | Aff,  One  -> Some One
  | One,  Aff  -> Some One
  | Aff,  Zero -> Some Zero
  | Zero, Aff  -> Some Zero
  | _          -> if u = u' then Some u else None

let join_usage u u' = 
  let open Util.Option in 
  match u, u' with
  | Int, Int               -> return Int
  | Lin(u, t), Lin(u', t') -> join_usage_lin u u' >>= fun u'' -> 
                              if t = t' then return (Lin(u'', t)) else assert false
  | _                      -> none

let join_hyp oldh newh = 
  let open Util.Option in 
  match oldh, newh with 
  | Tm(_, u), Tm(tp, u') -> join_usage u u' |> map (fun u'' -> Tm(tp, u''))
  | _                    -> return newh

let rec compatible oldctx newctx = 
  merge join_hyp (fun x -> Usage x) oldctx newctx

let parallel c1 c2 = 
  get_ctx >>= fun old -> 
  c1 >>= fun v1 -> 
  get_ctx >>= fun ctx1 -> 
  reset_usage old ctx1 >>= fun old' -> 
  set_ctx old' >>= fun () -> 
  c2 >>= fun v2 -> 
  get_ctx >>= fun ctx2 -> 
  compatible ctx1 ctx2 >>= fun ctx -> 
  set_ctx ctx >>= fun () -> 
  return (v1, v2)

module Par = Util.Seq(Util.MkIdiom(struct
                                    type 'a s = 'a t 
                                    type 'a t = 'a s
                                    let map = map 
                                    let unit () = return () 
                                    let ( ** ) = parallel 
                                  end))

module Seq = Util.Seq(Util.Monoidal(M))
         



let evar kind = 
  gensym "?a" >>= fun a -> 
  get_loc >>= fun loc -> 
  get_ctx >>= fun ctx -> 
  set_ctx ((a, (Tp(Exist, kind, None), loc)) :: ctx) >>= fun () -> 
  return a 

let inst x tm = 
  let rec loop = function
    | [] -> error (Unbound x)
    | (y, (Tp(Exist, k, None), loc)) :: ctx when x = y -> return ((x, (Tp(Exist, k, Some tm), loc)) :: ctx)
    | (y, h) :: ctx when x = y -> error (NotEvar x)
    | (y, h) :: ctx -> loop ctx >>= fun ctx -> 
                       return ((y, h) :: ctx)
  in 
  get_ctx >>= fun ctx -> 
  loop ctx >>= fun ctx -> 
  set_ctx ctx

let run ctx loc (Cmd cmd) = 
  let state = { ctx = ctx; loc = loc; gensym = 0 } in 
  Result.map fst Fn.id (cmd state)
