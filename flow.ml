(* a weird sort of recursive dataflow system? *)
module type FlowKey = sig
    include Map.OrderedType
    val to_string : t -> string (* for debugging *)
end

module type FLOW = sig
    type key
    type 'a exp

    (* applicative structure *)
    val pure : 'a -> 'a exp
    val ap : ('a -> 'b) exp -> 'a exp -> 'b exp
    val map : ('a -> 'b) -> 'a exp -> 'b exp (* convenience *)

    (* lazily computing a fixed-point map *)
    val fix : (key -> 'v) (* bottom value for each key *)
              -> ((key -> 'v exp) -> key -> 'v exp)
              -> ('v -> string)
              -> key -> 'v
end

module Flow(Key: FlowKey) : (FLOW with type key = Key.t) = struct
    type key = Key.t
    module S = Set.Make(Key)
    module M = Map.Make(Key)
    type keyset = S.t
    type 'a map = 'a M.t

    (* takes: frozen
     * returns: (value, changed, visited) *)
    type 'a result = 'a * bool * keyset
    type 'a exp = keyset -> 'a result

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

    (* my god, what have I done *)
    let fix (init: key -> 'v)
            (func: (key -> 'v exp) -> key -> 'v exp)
            (value_to_string: 'v -> string)
        : key -> 'v =
      let open Printf in
      let cache: 'v map ref = ref M.empty in
      let put k v = cache := M.add k v !cache in
      let get k = try M.find k !cache
                  with Not_found -> let v = init k in (put k v; v) in

      (* visit function *)
      let rec visit key frozen : 'v result =
        let cached_value = get key in
        let () = printf "visiting %s, current value %s\n%!"
                        (Key.to_string key) (value_to_string cached_value) in
        if S.mem key frozen
        (* NB: The only reason we return (S.singleton key) here is so that the
         * check for (not (S.mem key visited)) below, in `loop', works
         * properly. If we removed that we could return S.empty. *)
        then let () = print_endline " frozen!" in
             (cached_value, false, S.singleton key)
        else
          let () = print_endline " computing!" in
          let frozen = S.add key frozen in
          let recur () = func visit key frozen in
          let rec loop old_value changed_so_far =
            let () = printf "iterating %s\n%!" (Key.to_string key) in
            let (new_value, changed, visited) = recur () in
            let () = printf "iterated %s, old value %s, new value %s, changed %B\n%!"
                            (Key.to_string key)
                            (value_to_string old_value)
                            (value_to_string new_value)
                            changed in
            let changed = if old_value = new_value
                          then changed
                          else (put key new_value; true) in
            (* if nothing changed, we're done *)
            if not changed
            then (old_value, changed_so_far, S.add key visited)
            (* if we didn't depend on ourselves, no need to iterate *)
            else if not (S.mem key visited)
            then (new_value, changed_so_far || changed, S.add key visited)
            (* keep computing until we haven't changed anything. *)
            else loop new_value true
          in loop cached_value false in
      (* end of visit function *)

      let finished = ref S.empty in
      fun key -> let (value, _, visited) = visit key !finished in
                 let () = finished := S.union !finished visited in
                 value
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

let ex1 = I.fix (const 0) func1 string_of_int
