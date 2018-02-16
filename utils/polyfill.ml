
(* Simple Hashtbl wrapper to simulate a hashset *)
module Set : sig
  type 'a t = ('a, unit) Hashtbl.t
  val create : ?n:int -> ('a, unit) Hashtbl.t
  val exists : ('a, unit) Hashtbl.t -> 'a -> bool
  val difference : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val union : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val set_of_list : 'a list -> ('a, unit) Hashtbl.t
end = struct
  type 'a t = ('a, unit) Hashtbl.t
  let create ?(n=10) = Hashtbl.create n
  let exists set item =
    try
      let _ = Hashtbl.find set item in
      true
    with
    | Not_found -> false
  let difference set1 set2 =
    (* Copy set1 and add remove elements from set2 *)
    let set' = Hashtbl.copy set1 in
    Hashtbl.iter (fun item _ -> Hashtbl.remove set' item) set2;
    set'
  let union set1 set2 =
    (* Copy set1 and add all elements from set2 *)
    let set' = Hashtbl.copy set1 in
    Hashtbl.iter (fun item _ -> if exists set' item then () else Hashtbl.add set' item ()) set2;
    set'
  let set_of_list lst =
    let len = List.length lst in
    let set = create ~n:len in
    List.iter (fun item -> Hashtbl.add set item ()) lst;
    set
end
