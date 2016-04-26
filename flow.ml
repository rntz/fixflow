(* a weird sort of recursive dataflow system? *)
module type Applicative = sig
    type 'a exp
    val pure : 'a -> 'a exp
    val map : ('a -> 'b) -> 'a exp -> 'b exp
    val ap : ('a -> 'b) exp -> 'a exp -> 'b exp
end

module type FlowGraph = sig
    type node
    val node_compare : node -> node -> int
    val string_of_node : node -> string

    type value
    val value_equal : value -> value -> bool
    val string_of_value : value -> string

    val init: node -> value
    module MakeStep(A: Applicative) : sig
        val step: (node -> value A.exp) -> node -> value A.exp
    end
end

module Flow(G: FlowGraph): sig val get: G.node -> G.value
                               val clear: unit -> unit
                           end =
struct
    open G

    module Node = struct type t = node let compare = node_compare end
    module S = Set.Make(Node)
    module M = Map.Make(Node)
    type nodeset = S.t
    type 'a map = 'a M.t

    module Exp = struct
        (* an exp is a function
         * it takes: frozen
         * it returns: (value, changed, visited) *)
        type 'a result = 'a * bool * nodeset
        type 'a exp = nodeset -> 'a result

        let pure x _ = (x, false, S.empty)
        let ap f a frozen =
          let (func, f_changed, f_visited) = f frozen in
          (* note the sideways information passing: in addition to the nodes we
           * see as frozen, `a' sees `f_visited' as frozen. this is an
           * optimisation. *)
          let (arg, a_changed, a_visited) = a (S.union frozen f_visited) in
          (func arg,
           f_changed || a_changed,
           S.union f_visited a_visited)
        let map f a = ap (pure f) a
    end                         (* module Exp *)

    module Step = MakeStep(Exp)
    open Step

    (* ---------- DATAFLOW GRAPH IMPLEMENTATION ---------- *)
    let cache: value map ref = ref M.empty
    let finished = ref S.empty
    let clear () = (cache := M.empty; finished := S.empty)

    let put k v = cache := M.add k v !cache
    let get k = try M.find k !cache
                with Not_found -> let v = init k
                                  in (put k v; v)

    let rec get node =        (* `get' isn't recursive, but visit/iterate are *)
      let (value, _, visited) = visit node !finished in
      let () = finished := S.union !finished visited in
      value

    and visit (node: node) (frozen: nodeset): value result =
      let cached_value = get node in
      let () = printf "visiting %s, current value %s\n%!"
                      (string_of_node node) (string_of_value cached_value) in
      if S.mem node frozen
      then let () = print_endline " frozen!" in
           (* We return (S.singleton node) here so that the check for
            * (not (S.mem node visited)) below, in `loop', works. *)
           (cached_value, false, S.singleton node)
      else
        let () = print_endline " computing!" in
        iterate node (S.add node frozen) cached_value

    and iterate (node: node) (frozen: nodeset): value -> value result =
      let rec loop changed_so_far old_value =
        let () = printf "iterating %s\n%!" (string_of_node node) in
        let (new_value, changed, visited) = step visit node frozen in
        let () = printf "iterated %s, old value %s, new value %s, changed %B\n%!"
                        (string_of_node node)
                        (string_of_value old_value)
                        (string_of_value new_value)
                        changed in
        let changed = if old_value = new_value
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

end

(* instances *)
module S = Flow(struct type t = string
                       let compare = String.compare
                       let to_string x = x
                end)

module I = Flow(struct type t = int
                       let compare = compare
                       let to_string = string_of_int
                end)


(* utility *)
let const x y = x
let even (n:int): bool = n mod 2 = 0
let odd n = not (even n)


(* examples *)
(* let func1 (self: int -> int I.exp) (x: int): int I.exp =
 *   let prev = self x in
 *   match x with
 *   | 0 -> I.map (fun x -> if x < 2 then x + 1 else x) prev
 *   | n -> I.map ((+) 1) (self (n-1))
 * 
 * let ex1 = I.fix (const 0) func1 string_of_int *)
