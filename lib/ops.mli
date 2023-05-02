open Xla

module type T = sig
  type t

  val zeros_like : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val neg : t -> t
  val cos : t -> t
  val sin : t -> t
end

module Eval : T with type t = Xla.Op.t
