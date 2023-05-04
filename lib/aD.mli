type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a

val jvp
  :  (module Ops.T with type t = 'a)
  -> f:'a Forward.num diff
  -> v:'a
  -> 'a
  -> 'a * 'a

val vjp
  :  (module Ops.T with type t = 'a)
  -> f:'a Reverse.num diff
  -> v:'a
  -> 'a
  -> 'a * 'a

val value_and_grad
  :  (module Ops.T with type t = 'a)
  -> f:'a Reverse.num diff
  -> 'a
  -> 'a * 'a
