
(* Simple Hashtbl wrapper to implement immutability. *)
module Immutable_hashtbl : sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val create : int -> ('a, 'b) Hashtbl.t
  val find : ('a, 'b) Hashtbl.t -> 'a -> 'b
  val add : ('a, 'b) Hashtbl.t -> 'a -> 'b -> ('a, 'b) Hashtbl.t
  val remove : ('a, 'b) Hashtbl.t -> 'a -> ('a, 'b) Hashtbl.t
  val length : ('a, 'b) Hashtbl.t -> int
  val copy : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t
  val iter : ('a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
  val combine : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t
end = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t

  let create n = Hashtbl.create n

  let find tbl k = Hashtbl.find tbl k

  let add tbl k v =
    let tbl' = Hashtbl.copy tbl in
    Hashtbl.add tbl' k v;
    tbl'

  let remove tbl k =
    let tbl' = Hashtbl.copy tbl in
    Hashtbl.remove tbl' k;
    tbl'

  let length tbl = Hashtbl.length tbl

  let copy tbl = Hashtbl.copy tbl

  let iter fn tbl = Hashtbl.iter fn tbl

  let combine t1 t2 =
    let tbl = Hashtbl.copy t1 in
    Hashtbl.iter (fun k v -> Hashtbl.add tbl k v) t2;
    tbl
end

(* Simple Hashtbl wrapper to simulate a hashset. *)
module Set : sig
  type 'a t = ('a, unit) Hashtbl.t
  val create : int -> ('a, unit) Hashtbl.t
  val size : ('a, 'b) Hashtbl.t -> int
  val exists : ('a, unit) Hashtbl.t -> 'a -> bool
  val for_each : ('a, unit) Hashtbl.t -> ('a -> unit) -> unit
  val difference : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val remove : ('a, unit) Hashtbl.t -> 'a -> unit
  val add : ('a, unit) Hashtbl.t -> 'a -> unit
  val union : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val fold : ('a, unit) Hashtbl.t -> ('a -> 'c -> 'c) -> 'c -> 'c
  val set_of_list : 'a list -> ('a, unit) Hashtbl.t
end = struct
  type 'a t = ('a, unit) Hashtbl.t

  let create n = Hashtbl.create n

  let size set = Hashtbl.length set

  let exists set item =
    try
      let _ = Hashtbl.find set item in
      true
    with
    | Not_found -> false

  let for_each set fn = Hashtbl.iter (fun item _ -> fn item) set

  let difference set1 set2 =
    (* Copy set1 and add remove elements from set2 *)
    let set' = Hashtbl.copy set1 in
    Hashtbl.iter (fun item _ -> Hashtbl.remove set' item) set2;
    set'

  let remove set item = Hashtbl.remove set item

  let add set item = if exists set item then () else Hashtbl.add set item ()

  let union set1 set2 =
    (* Copy set1 and add all elements from set2 *)
    let set' = Hashtbl.copy set1 in
    Hashtbl.iter (fun item _ -> if exists set' item then () else Hashtbl.add set' item ()) set2;
    set'

  let fold set fn init = Hashtbl.fold (fun k _ acc -> fn k acc) set init

  let set_of_list lst =
    let len = List.length lst in
    let set = create len in
    List.iter (fun item -> Hashtbl.add set item ()) lst;
    set
end

(* Define a undirectional imperative graph of Select arguments. *)
module Interference_graph = struct
  module Graph_components = struct
    type t = Ast.Select.arg
  end

  (* Signature for creating a interference graph object. *)
  module G = Graph.Imperative.Graph.Abstract(Graph_components)

  (* Traversal module for our graph. *)
  module Bfs = Graph.Traverse.Bfs(G)

  (* Create an interference graph from a list of arguments.
   * Return that graph and also a hashmap linking Select variable args to its respective vertex.
   * We use this hashtable to create edges in our graph later. We need to keep track of the actual
   * graph vertex reference. *)
  let init (items : Ast.Select.arg list) : G.t * (Ast.Select.arg, G.vertex) Hashtbl.t =
    let n = List.length items in
    let vt = Hashtbl.create n in
    let graph = G.create () in
    List.iter (fun item ->
        let v = G.V.create item in
        Hashtbl.add vt item v;
        G.add_vertex graph v) items;
    graph, vt
end

(* Define the components we want in our max heap. *)
module Max_heap_components = struct
  type t = int * Interference_graph.Graph_components.t
  let compare a b =
    let (saturation_1, _vertex_1) = a in
    let (saturation_2, _vertex_2) = b in
    saturation_1 - saturation_2
end

(* Create max heap from binary heap functor. *)
module Max_heap = Binary_heap.Make(Max_heap_components)

(* Given a hashtable, count the number of unique values. *)
let count_unique tbl : int =
  let s = Set.create 53 in
  Hashtbl.iter (fun _k v -> Set.add s v) tbl;
  Set.size s

(* Global ID generator module. *)
module Dangerous_guid = struct
  let id = ref (-1)
  let get () = id := (!id + 1); !id
end
