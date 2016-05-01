(* a weird sort of recursive dataflow system *)

#use "debug.ml";;

(* utility *)
exception Unimplemented

let const x y = x
let option default opt = match opt with
  | None -> default
  | Some x -> x

let even (n:int): bool = n mod 2 = 0
let odd n = not (even n)

module ListUtils = struct
    let tabulate (keys: 'a list) (func: 'a -> 'b): ('a * 'b) list =
      List.map (fun k -> (k, func k)) keys

    let for_each (l: 'a list) (f: 'a -> unit): unit = ignore (List.map f l)
end

module type MAP_UTILS = sig
    include Map.S
    val of_list: (key * 'a) list -> 'a t
    val tabulate : key list -> (key -> 'a) -> 'a t
end

module MapUtils(M: Map.S)
       : (MAP_UTILS with type key = M.key
                    with type 'a t = 'a M.t) =
struct
    include M
    let of_list elts =
      let accum m (k,v) = add k v m in
      List.fold_left accum empty elts
    let tabulate keys f =
      let accum m k = add k (f k) m in
      List.fold_left accum empty keys
end


(* Signatures *)
module type FlowNode = sig
    include Map.OrderedType
    val to_string : t -> string (* for debugging *)
end

module type FLOW = sig
    type node
    type 'a exp

    (* applicative structure *)
    val pure : 'a -> 'a exp
    val map : ('a -> 'b) -> 'a exp -> 'b exp
    val ap : ('a -> 'b) exp -> 'a exp -> 'b exp

    (* lazily computing a fixed-point map
     * the map is only defined on nodes in `init'. *)
    val fix : init: (node * 'v) list
           -> step: ((node -> 'v exp) -> node -> 'v exp)
           -> ?value_eq: ('v -> 'v -> bool)
           -> ?value_to_string: ('v -> string)
           -> unit              (* side effect at this point *)
           -> (node -> 'v)
end


(* lazy pull-based dataflow with various optimizations *)
module Pull(Node: FlowNode) : (FLOW with type node = Node.t) = struct
    type node = Node.t
    module S = Set.Make(Node)
    module M = MapUtils(Map.Make(Node))
    type nodeset = S.t
    type 'a map = 'a M.t

    (* takes: frozen
     * returns: (value, changed, visited) *)
    type 'a result = 'a * bool * nodeset
    type 'a exp = nodeset -> 'a result

    let pure x _ = (x, false, S.empty)
    let ap f a frozen =
      let (func, f_changed, f_visited) = f frozen in
      (* note the sideways information passing: in addition to the nodes we see
       * as frozen, `a' sees `f_visited' as frozen.
       * in some sense this is an optimisation. *)
      let (arg, a_changed, a_visited) = a (S.union frozen f_visited) in
      (func arg,
       f_changed || a_changed,
       S.union f_visited a_visited)
    let map f a = ap (pure f) a

    let fix ~(init: (node * 'v) list)
            ~(step: (node -> 'v exp) -> node -> 'v exp)
            ?(value_eq: 'v -> 'v -> bool = (=))
            ?(value_to_string: 'v -> string = (fun _ -> "<abstract>"))
            () : node -> 'v =
      let module Graph = struct
          open Printf

          let cache: 'v map ref = ref (M.of_list init)
          let finished = ref S.empty

          let put k v = cache := M.add k v !cache
          let get k = M.find k !cache

          let rec compute node =
            let (value, _, visited) = visit node !finished in
            let () = finished := S.union !finished visited in
            value

          and visit node frozen : 'v result =
            let cached_value = get node in
            let () = printf "visiting %s, current value %s\n%!"
                            (Node.to_string node)
                            (value_to_string cached_value) in
            if S.mem node frozen
            (* NB: we return (S.singleton node) here so that the check for
             * (not (S.mem node visited)) below, in `loop', works. *)
            then let () = print_endline " frozen!" in
                 (cached_value, false, S.singleton node)
            else let () = print_endline " computing!" in
                 iterate node (S.add node frozen) cached_value

          and iterate node frozen =
            let rec loop changed_so_far old_value =
              let () = printf "iterating %s\n%!" (Node.to_string node) in
              let (new_value, changed, visited) = step visit node frozen in
              let () =
                printf "iterated %s, old value %s, new value %s, changed %B\n%!"
                       (Node.to_string node)
                       (value_to_string old_value)
                       (value_to_string new_value)
                       changed in
              let changed = if value_eq old_value new_value
                            then changed
                            else (put node new_value; true) in
              (* if nothing changed, we're done *)
              if not changed
              then (old_value, changed_so_far, S.add node visited)
              (* if we didn't depend on ourselves, no need to iterate *)
              else if not (S.mem node visited)
              then (new_value, changed_so_far || changed, S.add node visited)
              (* keep computing until we haven't changed anything. *)
              else loop true new_value
            in loop false

        end in
      Graph.compute
end


(* push-based dataflow with dirty-clean marking *)
module Push(Node: FlowNode) : (FLOW with type node = Node.t) = struct
    type node = Node.t
    module S = Set.Make(Node)
    module M = MapUtils(Map.Make(Node))
    type nodeset = S.t
    type 'a map = 'a M.t

    (* an expression is:
     * - a set of nodes it depends on
     * - a thunk that computes it
     *
     * Note that this, unlike the definition of 'a exp used in Pull, does not
     * form a monad!
     *)
    type 'a exp = nodeset * (unit -> 'a)

    let pure x = (S.empty, fun () -> x)
    let map f (adeps, a) = (adeps, fun () -> f (a ()))
    let ap (fdeps,f) (adeps,a) = (S.union fdeps adeps, fun () -> f () (a ()))

    let fix ~(init: (node * 'v) list)
            ~(step: (node -> 'v exp) -> node -> 'v exp)
            ?(value_eq: 'v -> 'v -> bool = (=))
            ?(value_to_string: 'v -> string = (fun _ -> "<abstract>"))
            () : node -> 'v =
      let module Graph = struct
          open Debug
          let nodes = List.map fst init
          let state: 'v map ref = ref (M.of_list init)
          let dirty: nodeset ref = ref (S.of_list nodes)

          let get node = M.find node !state
          let read (n: node): 'v exp = (S.singleton n, fun () -> get n)

          let exprs: 'v exp map = M.tabulate nodes (fun node -> step read node)

          (* okay, so now we have our "depends-on"/pull graph
           * now we invert it, to get the "supplies-to"/push graph *)
          let client_graph: nodeset map =
            let add_all node (deps, _) graph =
              (let graph' = M.tabulate (S.elements deps)
                                       (const (S.singleton node)) in
               let merge _ l r =
                 Some (S.union (option S.empty l) (option S.empty r)) in
               M.merge merge graph graph')
            in M.fold add_all exprs M.empty

          (* node --> set of "client" nodes that depend on it *)
          let clients (n: node): nodeset = M.find n client_graph

          let update (node: node): unit =
            (let (_, thunk) = M.find node exprs in
             let value = thunk () in
             state := M.add node value !state;
             dirty := S.union !dirty (clients node))

          let rec loop () =
            (if S.is_empty !dirty then () else
               let node = S.choose !dirty in
               dirty := S.remove node !dirty;
               update node;
               loop ())

        end
      in Graph.loop (); Graph.get
end


(* instances *)
module S = Pull(struct type t = string
                       let compare = String.compare
                       let to_string x = x
                end)

module I = Pull(struct type t = int
                       let compare = compare
                       let to_string = string_of_int
                end)


(* examples *)
(* f 0 = if f 0 < 2 then f 0 + 1 else f 0
 * f n = 1 + f (n-1)
 *)
let func1 (self: int -> int I.exp) (x: int): int I.exp =
  let prev = self x in
  match x with
  | 0 -> I.map (fun x -> if x < 2 then x + 1 else x) prev
  | n -> I.map ((+) 1) (self (n-1))

let ex1 = I.fix ~init:[0,0; 1,0; 2,0; 3,0]
                ~step:func1
                ~value_to_string:string_of_int ()
