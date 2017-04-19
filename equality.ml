(* Implementation of type equality *)

open Util
open Ast
open Context



let rec expand a vars kind tp = 
  let open Constr in
  let exp e = before a (evar kind >>= fun b -> 
                        expand b vars kind e >>= fun () -> 
                        return (var b @ loc e))
  in
  out tp >>= function 
  | Var x        -> expand_var a vars kind x 
  | App (e, e')  -> 
  | Forall(e,e') -> 
  | Exists(e,e') -> 
  | Lolli (e,e') -> exp e >>= fun b -> 
                    exp e' >>= fun c -> 
                    inst a (get (lolli (b, c)) (loc tp))
  | Tensor es    -> Seq.list (List.map exp es) >>= fun bs -> 
                    inst a (get (tensor bs) (loc tp))
  | With nes     -> let (ns, es) = List.split nes in 
                    Seq.list (List.map exp es) >>= fun bs ->
                    inst a (get (witht (List.combine ns bs)) (loc tp))
  | Sum ces      -> let (cs, es) = List.split ces in 
                    Seq.list (List.map exp es) >>= fun bs ->
                    inst a (get (sum (List.combine cs bs)) (loc tp))
  | Mu e         -> Mu (f e)
  | F e          -> F (f e)
  | G e          -> G (f e)
  | Always e     -> exp e >>= fun b -> 
                    inst a (get (always b) (loc tp))
  | Event e      -> exp e >>= fun b -> 
                    inst a (get (event b) (loc tp))
  | Record _     
  | Proj (_, _)  
  | Tuple _       
  | Con _        
  | Case (_,_) 
  | Annot (_,_) 
  | Num _       
  | Select _
  | Yield _ 
  | Abs (_,_)    
  | Lam (_,_)    
  | Type         
  | Linear       -> assert false 

and expand_var a vars kind x =   
  lookup x >>= function
  | Tm (_,_) -> error 
  | Tp (_,_,_) -> (??)
  | Mark  -> (??)
  | Hide _ -> (??)

