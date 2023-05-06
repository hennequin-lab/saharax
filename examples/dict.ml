open Base
open Saharax
module OAD = Owl.Algodiff.D

let msg s = s |> Sexp.to_string_hum |> Stdio.print_endline

(* start by defining a parameter dictionary *)

module A = struct
  type 'a t =
    { prm1 : 'a
    ; prm2 : 'a
    }
  [@@deriving dict]
end

module B = Dict.List (A.Make (AD.P))

let f (type t) (module O : Ops.T with type t = t) (x : t B.t) =
  let g (x : t A.t) = O.(cos x.prm1 * sin x.prm2) in
  List.fold x ~init:None ~f:(fun accu xi ->
    match accu with
    | None -> Some (g xi)
    | Some a -> Some O.(a + g xi))
  |> Option.value_exn

let x : OAD.t B.t =
  A.
    [ { prm1 = OAD.F 1.; prm2 = OAD.F 2. }
    ; { prm1 = OAD.F 3.; prm2 = OAD.F 4. }
    ; { prm1 = OAD.F 5.; prm2 = OAD.F 6. }
    ]

let fx, dfx =
  let module AD = AD.Make (B) (AD.P) (Backend.OWL) in
  AD.vjp ~f x
