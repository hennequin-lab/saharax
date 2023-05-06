open Base
open Saharax
module OAD = Owl.Algodiff.D
module AD = AD.Make (AD.P) (AD.P)

let msg s = s |> Sexp.to_string_hum |> Stdio.print_endline
let f (type t) (module O : Ops.T with type t = t) x = O.(sin (cos (x + x)))
let x = OAD.F 1.

let _, df_fwd =
  let module D = AD (Backend.OWL) in
  D.jvp ~f x

let _, df_rev =
  let module D = AD (Backend.OWL) in
  D.vjp ~f x

let df_pred = OAD.Maths.(F (-2.) * sin (x + x) * cos (cos (x + x)))

let _ =
  msg
    [%message
      "compare"
        ~fwd:(OAD.unpack_flt df_fwd : float)
        ~rev:(OAD.unpack_flt df_rev : float)
        ~ground_truth:(OAD.unpack_flt df_pred : float)]

(* second derivative via forward-on-reverse and reverse-on-reverse *)
let d2f, d2f' =
  (* inner is reverse *)
  let df (type t) (module O : Ops.T with type t = t) x =
    let module D = AD (O) in
    snd (D.vjp ~f x)
  in
  let module D = AD (Backend.OWL) in
  snd (D.jvp ~f:df x), snd (D.vjp ~f:df x)

(* second derivative via reverse-on-forward and forward-on-forward *)
and d2f'', d2f''' =
  (* inner is forward *)
  let df (type t) (module O : Ops.T with type t = t) x =
    let module D = AD (O) in
    snd (D.jvp ~f x)
  in
  let module D = AD (Backend.OWL) in
  snd (D.vjp ~f:df x), snd (D.jvp ~f:df x)

let _ =
  msg
    [%message
      "compare"
        ~fwd_rev:(OAD.unpack_flt d2f : float)
        ~fwd_fwd:(OAD.unpack_flt d2f' : float)
        ~rev_fwd:(OAD.unpack_flt d2f'' : float)
        ~rev_rev:(OAD.unpack_flt d2f''' : float)]

