module type T = sig
  type t

  val name : string
  val zeros_like : t -> t
  val ones_like : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val neg : t -> t
  val cos : t -> t
  val sin : t -> t
  val l2norm_sqr' : t -> t
end

module XLA_backend : T with type t = Xla.Op.t
module Owl_backend : T with type t = Owl.Algodiff.D.t
