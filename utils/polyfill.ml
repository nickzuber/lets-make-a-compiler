
(* Simple Hashtbl wrapper to simulate a hashset *)
module Set : sig
  type 'a t = ('a, unit) Hashtbl.t
  val create : int -> ('a, unit) Hashtbl.t
  val exists : ('a, unit) Hashtbl.t -> 'a -> bool
  val for_each : ('a, unit) Hashtbl.t -> ('a -> unit) -> unit
  val difference : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val union : ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t -> ('a, unit) Hashtbl.t
  val length : ('a, unit) Hashtbl.t -> int
  val set_of_list : 'a list -> ('a, unit) Hashtbl.t
end = struct
  type 'a t = ('a, unit) Hashtbl.t
  let create n = Hashtbl.create n
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
  let union set1 set2 =
    (* Copy set1 and add all elements from set2 *)
    let set' = Hashtbl.copy set1 in
    Hashtbl.iter (fun item _ -> if exists set' item then () else Hashtbl.add set' item ()) set2;
    set'
  let length set = Hashtbl.length set
  let set_of_list lst =
    let len = List.length lst in
    let set = create len in
    List.iter (fun item -> Hashtbl.add set item ()) lst;
    set
end

module InterferenceGraph = struct
  (* ocamlgraph *)
  open Graph

  (* Signature for creating a interference graph object *)
  module G = Imperative.Graph.Abstract(struct type t = Ast.Select.arg end)

  (* Traversal module for our graph *)
  module Bfs = Graph.Traverse.Bfs(G)

  (* Create an interference graph from a list of arguments.
     Return that graph and also a hashmap linking Select variable args to its respective vertex. *)
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
