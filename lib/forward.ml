(* primal / tangent pair *)
type 'a num =
  { p : 'a
  ; t : 'a
  }

module Make (O : Ops.T) = struct
  type nonrec t = O.t num

  open O

  let primal x = x.p
  let tangent x = x.t

  let lift ?tangent x =
    { p = x
    ; t =
        (match tangent with
         | Some v -> v
         | None -> O.ones_like x)
    }

  let zeros_like x = { p = O.zeros_like x.p; t = O.zeros_like x.p }
  let ones_like x = { p = O.ones_like x.p; t = O.zeros_like x.p }

  let neg x = { p = neg x.p; t = neg x.t }
  and ( + ) x y = { p = x.p + y.p; t = x.t + y.t }
  and ( * ) x y = { p = x.p * y.p; t = (x.p * y.t) + (y.p * x.t) }
  and cos x = { p = cos x.p; t = neg (x.t * sin x.p) }
  and sin x = { p = sin x.p; t = x.t * cos x.p }
  and l2norm_sqr' _ = assert false
end
