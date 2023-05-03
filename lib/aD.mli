type 'a diff = (module Ops.T with type t = 'a) -> 'a -> 'a
type differentiable = { f : 'a. (module Ops.T with type t = 'a) -> 'a -> 'a }

val jvp : (module Ops.T with type t = 'a) -> f:differentiable -> v:'a -> 'a -> 'a * 'a
val vjp : (module Ops.T with type t = 'a) -> f:differentiable -> v:'a -> 'a -> 'a * 'a
val value_and_grad : (module Ops.T with type t = 'a) -> f:differentiable -> 'a -> 'a * 'a
