open Base
open Saharax
module OAD = Owl.Algodiff.D

let f (type t) (module O : Ops.T with type t = t) x = O.(sin (cos (x + x)))
let x = OAD.F 1.
let _, df = AD.value_and_grad (module Ops.Owl_backend) ~f:{ f } x
let df_pred = OAD.Maths.(F (-2.) * sin (x + x) * cos (cos (x + x)))

let _ =
  Stdio.printf "\ngrad = %f | pred = %f\n%!" (OAD.unpack_flt df) (OAD.unpack_flt df_pred)

(* try computing the second derivative by nesting forward and reverse... *)
let d2f =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.value_and_grad (module O) ~f:{ f } x)
  in
  snd (AD.jvp (module Ops.Owl_backend) ~f:{ f = df } ~v:(OAD.F 1.) x)

(* ... and by nesting forward and forward *)
and d2f' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.jvp (module O) ~f:{ f } ~v:(O.ones_like x) x)
  in
  snd (AD.jvp (module Ops.Owl_backend) ~f:{ f = df } ~v:(OAD.F 1.) x)

(* ... and by nesting reward and forward... *)
and d2f'' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.jvp (module O) ~f:{ f } ~v:(O.ones_like x) x)
  in
  snd (AD.value_and_grad (module Ops.Owl_backend) ~f:{ f = df } x)

(* ... and by nesting reverse and reverse *)
and d2f''' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.value_and_grad (module O) ~f:{ f } x)
  in
  snd (AD.value_and_grad (module Ops.Owl_backend) ~f:{ f = df } x)

let _ =
  Stdio.printf
    "grad2 (fwd + rev) = %f\n\
     grad2 (fwd + fwd) = %f\n\
     grad2 (rev + fwd) = %f\n\
     grad2 (rev + rev) = %f\n\
     %!"
    (OAD.unpack_flt d2f)
    (OAD.unpack_flt d2f')
    (OAD.unpack_flt d2f'')
    (OAD.unpack_flt d2f''')
