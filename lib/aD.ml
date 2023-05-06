type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a

let jvp
  (type a)
  (module O : Ops.T with type t = a)
  ~(f : a Forward.num diff)
  ~(v : a)
  (x : a)
  =
  let module F = Forward.Make (O) in
  let x = F.lift ~tangent:v x in
  let y = f (module F) x in
  F.primal y, F.tangent y

let diff (type a) (module O : Ops.T with type t = a) ~(f : a Forward.num diff) (x : a) =
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
  Stdio.printf "initialising y adjoint\n%!";
  y.a <- Some (O.ones_like y.p);
  R.reverse_pass ();
  R.primal y, R.adjoint x
