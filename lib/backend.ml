module type T = sig
  val name : string

  include Ops.T
end

module XLA = struct
  let name = "XLA"

  type t = Xla.Op.t

  let zeros_like = Xla.Op.zeros_like
  let ones_like (_ : t) : t = assert false
  let ( + ) = Xla.Op.add
  let ( * ) = Xla.Op.mul
  let neg = Xla.Op.neg
  let sin = Xla.Op.cos
  let cos = Xla.Op.cos
  let l2norm_sqr' _ = assert false
end

module OWL = struct
  let name = "Owl"

  module A = Owl.Algodiff.D

  type t = A.t

  let zeros_like = A.zero

  let ones_like x =
    match x with
    | A.F _ -> A.F 1.
    | A.Arr _ -> A.Arr.(ones (shape x))
    | _ -> assert false

  let ( + ) = A.Maths.( + )
  let ( * ) = A.Maths.( * )
  let neg = A.Maths.neg
  let sin = A.Maths.sin
  let cos = A.Maths.cos
  let l2norm_sqr' = A.Maths.l2norm_sqr'
end
