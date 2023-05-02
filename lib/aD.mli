type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a
type differentiable = { f : 'a. (module Ops.T with type t = 'a) -> 'a -> 'a }

val jvp
  :  (module Ops.T with type t = 'a)
  -> f:differentiable
  -> v:'a
  -> 'a
  -> 'a Forward.num

val vjp
  :  (module Ops.T with type t = 'a)
  -> f:differentiable
  -> v:'a
  -> 'a
  -> 'a Reverse.num
