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

module Make (B : Basic) : T with type 'a t = 'a B.t = struct
  include B

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

module List (P : T) = Make (struct
  type 'a t = 'a P.t list

  let map x ~f = List.map x ~f:(P.map ~f)
  let map2 x y ~f = List.map2_exn x y ~f:(P.map2 ~f)

  let fold ?path x ~init ~f =
    List.foldi x ~init ~f:(fun i init w ->
      P.fold ?path:(cat (Int.to_string i) path) ~init ~f w)

  let fold2 ?path x y ~init ~f =
    let _, result =
      List.fold2_exn x y ~init:(0, init) ~f:(fun (i, init) w1 w2 ->
        i + 1, P.fold2 ?path:(cat (Int.to_string i) path) ~init ~f w1 w2)
    in
    result
end)

module Array (P : T) : T with type 'a t = 'a P.t array = Make (struct
  type 'a t = 'a P.t array

  let map x ~f = Array.map x ~f:(P.map ~f)
  let map2 x y ~f = Array.map2_exn x y ~f:(P.map2 ~f)

  let fold ?path x ~init ~f =
    Array.foldi x ~init ~f:(fun i init w ->
      P.fold ?path:(cat (Int.to_string i) path) ~init ~f w)

  let fold2 ?path x y ~init ~f =
    let _, result =
      Array.fold2_exn x y ~init:(0, init) ~f:(fun (i, init) w1 w2 ->
        i + 1, P.fold2 ?path:(cat (Int.to_string i) path) ~init ~f w1 w2)
    in
    result
end)
