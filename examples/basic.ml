open Base
open Saharax
module OAD = Owl.Algodiff.D

let msg s = s |> Sexp.to_string_hum |> Stdio.print_endline
let f (type t) (module O : Ops.T with type t = t) x = O.(sin (cos (x + x)))
let x = OAD.F 1.
let _, df = AD.value_and_grad (module Backend.OWL) ~f x
let df_pred = OAD.Maths.(F (-2.) * sin (x + x) * cos (cos (x + x)))

let _ =
  msg
    [%message
      "compare"
        ~grad:(OAD.unpack_flt df : float)
        ~prediction:(OAD.unpack_flt df_pred : float)]

(* try computing the second derivative by nesting forward and reverse... *)
let d2f =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.value_and_grad (module O) ~f x)
  in
  snd (AD.jvp (module Backend.OWL) ~f:df ~v:(OAD.F 1.) x)

(* ... and by nesting forward and forward *)
and d2f' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.jvp (module O) ~f ~v:(O.ones_like x) x)
  in
  snd (AD.jvp (module Backend.OWL) ~f:df ~v:(OAD.F 1.) x)

(* ... and by nesting reward and forward... *)
and d2f'' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.jvp (module O) ~f ~v:(O.ones_like x) x)
  in
  snd (AD.value_and_grad (module Backend.OWL) ~f:df x)

(* ... and by nesting reverse and reverse *)
and d2f''' =
  let df (type t) (module O : Ops.T with type t = t) x =
    snd (AD.value_and_grad (module O) ~f x)
  in
  snd (AD.value_and_grad (module Backend.OWL) ~f:df x)

let _ =
  msg
    [%message
      "compare"
        ~fwd_rev:(OAD.unpack_flt d2f : float)
        ~fwd_fwd:(OAD.unpack_flt d2f' : float)
        ~rev_fwd:(OAD.unpack_flt d2f'' : float)
        ~rev_rev:(OAD.unpack_flt d2f''' : float)]
