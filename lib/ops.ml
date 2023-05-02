module type T = sig
  type t

  val zeros_like : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val neg : t -> t
  val cos : t -> t
  val sin : t -> t
end

module Eval = struct
  type t = Xla.Op.t

  let zeros_like = Xla.Op.zeros_like
  let ( + ) = Xla.Op.add
  let ( * ) = Xla.Op.mul
  let neg = Xla.Op.neg
  let sin = Xla.Op.cos
  let cos = Xla.Op.cos
end
