open Base

type 'a num =
  { p : 'a
  ; mutable a : 'a Option.t
  }

module Make (O : Ops.T) : sig
  include Ops.T with type t = O.t num

  val lift : ?adjoint:O.t -> O.t -> t
  val reverse_pass : unit -> unit
  val primal : t -> O.t
  val adjoint : t -> O.t
  val set_adjoint : t -> O.t -> unit
end
