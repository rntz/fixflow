(* a weird sort of recursive dataflow system? *)
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

    (* lazily computing a fixed-point map *)
    val fix : ?value_eq: ('v -> 'v -> bool)
              -> ?value_to_string: ('v -> string)
              -> init: (node -> 'v)
              -> step: ((node -> 'v exp) -> node -> 'v exp)
              -> unit           (* side effect at this point *)
              -> (node -> 'v)
end

module Flow(Node: FlowNode) : (FLOW with type node = Node.t) = struct
    type node = Node.t
    module S = Set.Make(Node)
    module M = Map.Make(Node)
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

    (* this is one hot mess *)
    let fix ?(value_eq: 'v -> 'v -> bool = (=))
            ?(value_to_string: 'v -> string = (fun _ -> "<abstract>"))
            ~(init: node -> 'v)
            ~(step: (node -> 'v exp) -> node -> 'v exp)
            () : node -> 'v =
      let module Graph = struct
          open Printf

          let cache: 'v map ref = ref M.empty
          let finished = ref S.empty

          let put k v = cache := M.add k v !cache
          let get k = try M.find k !cache
                      with Not_found -> let v = init k
                                        in (put k v; v)

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
let func1 (self: int -> int I.exp) (x: int): int I.exp =
  let prev = self x in
  match x with
  | 0 -> I.map (fun x -> if x < 2 then x + 1 else x) prev
  | n -> I.map ((+) 1) (self (n-1))

let ex1 = I.fix ~init:(const 0) ~step:func1 ~value_to_string:string_of_int ()
