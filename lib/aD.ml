module P = Dict.P

module Make (In : Dict.T) (Out : Dict.T) (O : Ops.T) = struct
  let jvp
    ~(f :
       (module Ops.T with type t = O.t Forward.num)
       -> O.t Forward.num In.t
       -> O.t Forward.num Out.t)
    ?(v : O.t In.t option)
    (x : O.t In.t)
    =
    let module F = Forward.Make (O) in
    let v =
      match v with
      | None -> In.map x ~f:(fun x -> O.ones_like x)
      | Some v -> v
    in
    let x = In.map2 x v ~f:(fun x v -> F.lift ~tangent:v x) in
    let y = f (module F) x in
    Out.(map y ~f:F.primal, map y ~f:F.tangent)

  let vjp
    ~(f :
       (module Ops.T with type t = O.t Reverse.num)
       -> O.t Reverse.num In.t
       -> O.t Reverse.num Out.t)
    ?(v : O.t Out.t option)
    (x : O.t In.t)
    =
    let module R = Reverse.Make (O) in
    let x = In.map x ~f:R.lift in
    let y = f (module R) x in
    let v =
      match v with
      | None -> Out.map y ~f:(fun y -> O.ones_like y.p)
      | Some v -> v
    in
    Out.iter2 y v ~f:(fun y v -> y.a <- Some v);
    R.reverse_pass ();
    Out.map y ~f:R.primal, In.map x ~f:R.adjoint
end

(*   let diff (type a) (module O : Ops.T with type t = a) ~(f : a Forward.num diff) (x : a) =
    let module F = Forward.Make (O) in
    let x = F.lift x in
    let y = f (module F) x in
    F.primal y, F.tangent y

  let vjp
    (type a)
    (module O : Ops.T with type t = a)
    ~(f : a Reverse.num diff)
    ~(v : a)
    (x : a)
    =
    let module R = Reverse.Make (O) in
    let x = R.lift x in
    let y = f (module R) x in
    y.a <- Some v;
    R.reverse_pass ();
    R.primal y, R.adjoint x

  let value_and_grad
    (type a)
    (module O : Ops.T with type t = a)
    ~(f : a Reverse.num diff)
    (x : a)
    =
    let module R = Reverse.Make (O) in
    let x = R.lift x in
    let y = f (module R) x in
    y.a <- Some (O.ones_like y.p);
    R.reverse_pass ();
    R.primal y, R.adjoint x
end
*)
