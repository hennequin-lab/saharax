open Base
open Saharax
module OAD = Owl.Algodiff.D

let msg s = s |> Sexp.to_string_hum |> Stdio.print_endline
let f (type t) (module O : Ops.T with type t = t) x = O.(sin (cos (x + x)))
let x = OAD.F 1.
let _, df_fwd = AD.diff (module Backend.OWL) ~f x
let _, df_rev = AD.value_and_grad (module Backend.OWL) ~f x
let df_pred = OAD.Maths.(F (-2.) * sin (x + x) * cos (cos (x + x)))

let _ =
  msg
    [%message
      "compare"
        ~fwd:(OAD.unpack_flt df_fwd : float)
        ~rev:(OAD.unpack_flt df_rev : float)
        ~ground_truth:(OAD.unpack_flt df_pred : float)]

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

module MLP_layer = struct
  type 'a t =
    { w : 'a
    ; b : 'a
    }
  [@@deriving dict]
end

module MLP = struct
  type ('a, 'b) t =
    { l1 : 'a
    ; l2 : 'b
    }
  [@@deriving dict]
end

module P = MLP.Make (Dict.P) (MLP_layer.Make (Dict.P))

let x1 : float P.t = { l1 = 0.; l2 = { w = 0.; b = 0. } }
let x2 : float P.t = { l1 = 2.; l2 = { w = 3.; b = -2. } }
