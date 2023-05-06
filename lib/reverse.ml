open Base

(* primal / adjoint pair *)
type 'a num =
  { p : 'a
  ; mutable a : 'a Option.t
  }

module Make (O : Ops.T) = struct
  type t = O.t num

  let s = Stack.create ()

  let rec reverse_pass () =
    match Stack.pop s with
    | Some u ->
      u ();
      reverse_pass ()
    | None -> ()

  let primal x = x.p

  let adjoint x =
    match x.a with
    | Some a -> a
    | None -> failwith "this value has no adjoint"

  let set_adjoint x a = x.a <- Some a

  let update_adj x xa_update =
    x.a
      <- (match x.a with
          | None -> Some xa_update
          | Some xa -> Some O.(xa + xa_update))

  let lift_11 f df x =
    let y = { p = f x.p; a = None } in
    let u () =
      Option.iter y.a ~f:(fun ya ->
        let xa_update = df x.p ya in
        update_adj x xa_update)
    in
    Stack.push s u;
    y

  let lift_21 f df x1 x2 =
    let y = { p = f x1.p x2.p; a = None } in
    let u () =
      Option.iter y.a ~f:(fun ya ->
        let x1a, x2a = df x1.p x2.p ya in
        update_adj x1 x1a;
        update_adj x2 x2a)
    in
    Stack.push s u;
    y

  let lift ?adjoint x =
    let a =
      match adjoint with
      | Some v -> Some v
      | None -> Some (O.zeros_like x)
    in
    { p = x; a }

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
    let f _ : O.t = assert false in
    let df _ _ = assert false in
    lift_11 f df
end
