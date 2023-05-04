open Base

type path = string list

let cat label =
  if String.(label = "")
  then Fn.id
  else
    function
    | None -> Some [ label ]
    | Some p -> Some (label :: p)

module type Basic = sig
  (** If you want to make your own custom parameter structure
      which for a reason or another cannot be automated using [ppx-prms]
      (e.g. if it is not a record type), the you will need to use {!Make}
      and provide a module of this {!Basic} type. *)

  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  (** [?path] optionally contains a record of the path (a list of strings, in reverse order) taken to
      arrive at the current value, if it is nested within a broader structure. Make sure to use this
      if you want to attach string labels to the various components of your custom ['a p] type
      (e.g. use for saving to files, see {!Prms.T.save_txt}). *)
  val fold : ?path:path -> 'a t -> init:'c -> f:('c -> 'a * path option -> 'c) -> 'c

  val fold2
    :  ?path:path
    -> 'a t
    -> 'b t
    -> init:'c
    -> f:('c -> 'a * 'b * path option -> 'c)
    -> 'c
end

module type T = sig
  (** Main type for parameter structures. *)

  include Basic

  val iter : 'a t -> f:('a -> unit) -> unit
  val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
end

module Make (P : Basic) : T with type 'a t = 'a P.t = struct
  include P

  let iter x ~f = fold ?path:None x ~init:() ~f:(fun () (x, _) -> f x)
  let iter2 x y ~f = fold2 ?path:None x y ~init:() ~f:(fun () (x, y, _) -> f x y)
end

module P : T with type 'a t = 'a = Make (struct
  type 'a t = 'a

  let map x ~f = f x
  let map2 x y ~f = f x y
  let fold ?path x ~init ~f = f init (x, path)
  let fold2 ?path x y ~init ~f = f init (x, y, path)
end)
