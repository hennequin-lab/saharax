open Base
module E = Caml.Effect

type 'a num =
  { p : 'a
  ; mutable a : 'a Option.t
  }

module Make (O : Ops.T) : sig
  include Ops.T with type t = O.t num

  type _ E.t += Shift : (('a, t) E.Deep.continuation -> t) -> 'a E.t

  val primal : t -> O.t
  val adjoint : t -> O.t
end
