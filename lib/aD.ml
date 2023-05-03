open Base
module E = Caml.Effect

type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a
type differentiable = { f : 'a. (module Ops.T with type t = 'a) -> 'a -> 'a }

let jvp (type a) (module O : Ops.T with type t = a) ~(f : differentiable) ~(v : a) (x : a)
  =
  let module F = Forward.Make (O) in
  let v = Forward.{ p = x; t = v } in
  let y = f.f (module F) v in
  y.p, y.t

let vjp (type a) (module O : Ops.T with type t = a) ~(f : differentiable) ~(v : a) (x : a)
  =
  let x = Reverse.{ p = x; a = Some (O.zeros_like x) } in
  let module R = Reverse.Make (O) in
  let y =
    E.Deep.try_with
      (fun () ->
        let y = f.f (module R) x in
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
  y.p, Option.value_exn x.a

let value_and_grad
  (type a)
  (module O : Ops.T with type t = a)
  ~(f : differentiable)
  (x : a)
  =
  let x = Reverse.{ p = x; a = Some (O.zeros_like x) } in
  let module R = Reverse.Make (O) in
  let y =
    E.Deep.try_with
      (fun () ->
        let y = f.f (module R) x in
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
  y.p, Option.value_exn x.a
