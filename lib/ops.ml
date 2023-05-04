module type T = sig
  type t

  val zeros_like : t -> t
  val ones_like : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val neg : t -> t
  val cos : t -> t
  val sin : t -> t
  val l2norm_sqr' : t -> t
end
