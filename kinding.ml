(* Kind checking, bidirectionally *)

open Util
open Ast
open Context

let rec kind_eq kind kind' = 
  match (Ast.out kind, Ast.out kind') with
  | Linear, Linear -> return () 
  | Type, Type -> return ()  
 | Arrow(k1, k2), Arrow(k1', k2') -> kind_eq k1 k1' >>= fun () -> 
                                      kind_eq k2 k2' 
  | _, _ -> error (NotEqual(kind, kind', "kind mismatch"))
  

let rec check_kind tp kind = 
  Seq.pair (out tp, out kind) >>= fun (t, k) -> 
  match t, k with
  | Lam (PVar, tbody), Arrow(k1, k2) -> 
     (out tbody >>= function
      | Abs(x, tbody) -> with_hyp (x, Tp(Univ, k1, None)) (check_kind tbody k2)
      | _ -> assert false)
  | Lam (PVar, tbody), _ -> error (Check_mismatch(kind, "function occurs here"))
  | Lam (p, _), _ -> assert false 
  | Forall(kind', tbody), _ 
  | Exists(kind', tbody), _ -> check_kind tbody (Ast.into (loc tp) (Arrow(kind', kind)))
  | Lolli (tp1, tp2), Linear -> check_kind tp1 kind >>= fun () -> 
                                check_kind tp2 kind 
  | Lolli (tp1, tp2), _ -> error (Check_mismatch(kind, "linear type occurs here"))
  | Arrow (tp1, tp2), Type -> check_kind tp1 kind >>= fun () -> 
                              check_kind tp2 kind 
  | Arrow (tp1, tp2), _   -> error (Check_mismatch(kind, "intuitionistic function type occurs here"))
  | Tensor tps, Linear -> Seq.list (List.map (fun tp -> check_kind tp kind) tps) >>= fun _ -> 
                          return () 
  | Tensor _, _        -> error (Check_mismatch(kind, "linear tensor product type occurs here"))
  | With nts, Linear -> Seq.list (List.map (fun (_, tp) -> check_kind tp kind) nts) >>= fun _ -> 
                        return () 
  | With _, _        -> error (Check_mismatch(kind, "linear cartesian product type occurs here"))
  | Sum cts, Linear -> Seq.list (List.map (fun (_, tp) -> check_kind tp kind) cts) >>= fun _ -> 
                       return () 
  | Sum cts, Type -> Seq.list (List.map (fun (_, tp) -> check_kind tp kind) cts) >>= fun _ -> 
                     return () 
  | Sum _, _      -> error (Check_mismatch(kind, "sum type constructor occurs here"))
  | Mu(kind', tbody), _ -> 
     kind_eq kind kind' >>= fun () -> 
     (out tbody >>= function
      | Abs(x, tbody) -> with_hyp (x, Tp(Univ, kind', None)) (check_kind tbody kind')
      | _ -> assert false)
  | F tp', Linear -> check_kind tp' (Ast.into (loc tp) Type)
  | F _, _        -> error (Check_mismatch(kind, "linear F type constructor occurs here"))
  | G tp', Type -> check_kind tp' (Ast.into (loc tp) Linear)
  | G _, _        -> error (Check_mismatch(kind, "linear G type constructor occurs here"))
  | Always tp', Linear -> check_kind tp' kind
  | Always tp', _      -> error (Check_mismatch(kind, "linear always type constructor occurs here"))
  | Event tp', Linear -> check_kind tp' kind
  | Event tp', _      -> error (Check_mismatch(kind, "linear event type constructor occurs here"))
  | Var _, _ 
  | Annot (_,_), _
  | App (_,_), _ -> synth_kind tp >>= fun kind' -> 
                    kind_eq kind kind'
  | Type, _  
  | Linear, _
  | Record _, _ 
  | Proj (_,_), _
  | Tuple _, _
  | Con _, _ 
  | Case (_,_), _
  | Num _, _ 
  | Select _, _ 
  | Yield _, _  
  | Abs (_,_), _ -> assert false 
                          
and synth_kind tp = 
  out tp >>= function
  | Var x -> 
     (lookup x >>= function
      | Context.Tm (_,_) -> error (IllSorted(x, "term variable occurring in type"))
      | Context.Tp (_,kind,_) -> return kind
      | Context.Mark  -> assert false
      | Context.Hide _ -> assert false)
  | App (tp1, tp2) -> 
     (synth_kind tp1 >>= fun k1 -> 
      out k1 >>= function 
      | Arrow(k2, kind) -> check_kind tp2 k2 >>= fun () -> 
                           return kind 
      | _ -> error (Synth_mismatch(k1, "function kind")))
  | Annot(tp, kind) -> check_kind tp kind >>= fun () -> 
                       return kind 
  | _ -> assert false 
  
  
