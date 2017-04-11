let fst3 (a, b, c) = a
let snd3 (a, b, c) = b 
let thd3 (a, b, c) = c 

module type MONAD = sig
  type 'a t 
  val map : ('a -> 'b) -> 'a t -> 'b t 
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
end

module type IDIOM = sig
  type 'a t 
  val map : ('a -> 'b) -> 'a t -> 'b t 
  val return : 'a -> 'a t
  val ( ** ) : 'a t -> 'b t -> ('a * 'b) t 
  val app : ('a -> 'b) t -> 'a t -> 'b t 
end
                      

module type SEQ = sig
  type 'a t 

  val pair : 'a t * 'b t -> ('a * 'b) t 
  val option : 'a t option -> 'a option t 
  val result : ('a t, 'b) result -> ('a,'b) result t 
  val list : 'a t list -> 'a list t
end 

module Idiom(M : MONAD) : IDIOM with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t

  open M 

  let map = M.map 

  let return = M.return 

  let ( ** ) m1 m2 = m1 >>= fun v1 -> 
                     m2 >>= fun v2 -> 
                     return (v1, v2)

  let app mf mv = mf >>= fun f -> 
                  mv >>= fun v -> 
                  return (f v)
end

module Seq(A : IDIOM) : SEQ with type 'a t = 'a A.t = struct
  type 'a t = 'a A.t

  open A

  let option = function
    | None -> return None
    | Some c -> app (return (fun v -> Some v)) c 

  let result = function 
    | Error v -> return (Error v)
    | Ok c -> app (return (fun v -> Ok v)) c

  let pair (c, c') = c ** c'
                  
  let cons (x, xs) = x :: xs 

  let rec list = function
    | [] -> return []
    | m :: ms -> map cons (m ** (list ms))
end
