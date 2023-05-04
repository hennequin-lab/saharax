module type T = sig
  val name : string

  include Ops.T
end

module XLA : T with type t = Xla.Op.t
module OWL : T with type t = Owl.Algodiff.D.t
