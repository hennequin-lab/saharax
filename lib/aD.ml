open Base
module E = Caml.Effect

type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a

let jvp (type a) (module O : Ops.T with type t = a) ~f ~(v : a) (x : a) =
  let open Forward in
  let module F = Make (O) in
  let v = { p = x; t = v } in
  let y = f (module F : Ops.T with type t = O.t Forward.num) v in
  F.primal y, F.tangent y

let vjp (type a) (module O : Ops.T with type t = a) ~f ~(v : a) (x : a) =
  let open Reverse in
  let x = { p = x; a = Some (O.zeros_like x) } in
  let module R = Make (O) in
  let y =
    E.Deep.try_with
      (fun () ->
        let y = f (module R : Ops.T with type t = O.t Reverse.num) x in
        y.a <- Some v;
        y)
      ()
      E.Deep.
        { effc =
            (fun (type a) (e : a E.t) ->
              match e with
              | R.Shift f -> Some f
              | _ -> None)
        }
  in
  R.primal y, R.adjoint x

let value_and_grad (type a) (module O : Ops.T with type t = a) ~f (x : a) =
  let open Reverse in
  let x = { p = x; a = Some (O.zeros_like x) } in
  let module R = Make (O) in
  let y =
    E.Deep.try_with
      (fun () ->
        let y = f (module R : Ops.T with type t = O.t Reverse.num) x in
        y.a <- Some (O.ones_like y.p);
        y)
      ()
      E.Deep.
        { effc =
            (fun (type a) (e : a E.t) ->
              match e with
              | R.Shift f -> Some f
              | _ -> None)
        }
  in
  R.primal y, R.adjoint x
