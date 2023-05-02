module E = Caml.Effect

type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a
type differentiable = { f : 'a. (module Ops.T with type t = 'a) -> 'a -> 'a }

let jvp (type a) (module O : Ops.T with type t = a) ~(f : differentiable) ~(v : a) (x : a)
  =
  let module F = Forward.Make (O) in
  let v = Forward.{ p = x; t = v } in
  let y = f.f (module F) v in
  (y : O.t Forward.num)

let vjp (type a) (module O : Ops.T with type t = a) ~(f : differentiable) ~(v : a) (x : a)
  =
  let module R = Reverse.Make (O) in
  E.Deep.try_with
    (fun () ->
      let y = f.f (module R) Reverse.{ p = x; a = Some (O.zeros_like x) } in
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
