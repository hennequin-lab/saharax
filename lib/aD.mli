module P : module type of Dict.P

module Make : functor (In : Dict.T) (Out : Dict.T) (O : Ops.T) -> sig
  val jvp
    :  f:
         ((module Ops.T with type t = O.t Forward.num)
          -> O.t Forward.num In.t
          -> O.t Forward.num Out.t)
    -> ?v:O.t In.t
    -> O.t In.t
    -> O.t Out.t * O.t Out.t

  val vjp
    :  f:
         ((module Ops.T with type t = O.t Reverse.num)
          -> O.t Reverse.num In.t
          -> O.t Reverse.num Out.t)
    -> ?v:O.t Out.t
    -> O.t In.t
    -> O.t Out.t * O.t In.t
end
