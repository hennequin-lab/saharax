type 'a num =
  { p : 'a
  ; t : 'a
  }

module Make (O : Ops.T) : sig
  include Ops.T with type t = O.t num

  val lift : ?tangent:O.t -> O.t -> t
  val primal : t -> O.t
  val tangent : t -> O.t
end
