open Base
module E = Caml.Effect

(* primal / adjoint pair *)
type 'a num =
  { p : 'a
  ; mutable a : 'a Option.t
  }

module Make (O : Ops.T) = struct
  type t = O.t num

  let primal x = x.p

  let adjoint x =
    match x.a with
    | Some a -> a
    | None -> failwith "this value has no adjoint"

  type _ E.t += Shift : (('a, t) E.Deep.continuation -> t) -> 'a E.t

  let update_adj x xa_update =
    x.a
      <- (match x.a with
          | None -> Some xa_update
          | Some xa -> Some O.(xa + xa_update))

  let lift_11 f df x =
    E.perform
      (Shift
         (fun (k : (t, t) E.Deep.continuation) ->
           let y = { p = f x.p; a = None } in
           let z = E.Deep.continue k y in
           Option.iter y.a ~f:(fun ya ->
             let xa_update = df x.p ya in
             update_adj x xa_update);
           z))

  let lift_21 f df x1 x2 =
    E.perform
      (Shift
         (fun (k : (t, t) E.Deep.continuation) ->
           let y = { p = f x1.p x2.p; a = None } in
           let z = E.Deep.continue k y in
           Option.iter y.a ~f:(fun ya ->
             let x1a, x2a = df x1.p x2.p ya in
             update_adj x1 x1a;
             update_adj x2 x2a);
           z))

  let zeros_like x = { p = O.zeros_like x.p; a = Some (O.zeros_like x.p) }
  let ones_like x = { p = O.ones_like x.p; a = Some (O.zeros_like x.p) }

  let neg =
    let f x = O.neg x in
    let df _ dy = O.neg dy in
    lift_11 f df

  let ( + ) =
    let f x1 x2 = O.(x1 + x2) in
    let df _ _ dy = dy, dy in
    lift_21 f df

  let ( * ) =
    let f x1 x2 = O.(x1 * x2) in
    let df x1 x2 dy = O.(x2 * dy, x1 * dy) in
    lift_21 f df

  let cos =
    let f x = O.cos x in
    let df x dy = O.(neg dy * sin x) in
    lift_11 f df

  let sin =
    let f x = O.sin x in
    let df x dy = O.(dy * cos x) in
    lift_11 f df

  let l2norm_sqr' =
    let f _ = assert false in
    let df _ _ = assert false in
    lift_11 f df
end
