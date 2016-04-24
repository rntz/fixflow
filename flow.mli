(* a weird sort of recursive dataflow system? *)
module type FLOW = sig
    type key
    type 'a exp

    (* applicative structure *)
    val pure : 'a -> 'a exp
    val (~) : ('a -> 'b) exp -> 'a exp -> 'b exp

    (* lazily computing a fixed-point map *)
    val fix : (key -> 'v) (* bottom value for each key *)
              -> ((key -> 'v exp) -> key -> 'v exp)
              -> key -> 'v
end;;

module Flow(Key: Map.OrderedType) : (FLOW with type key = Key.t) = struct
    type key = Key.t
    module S = Set.Make(Key)
    module M = Map.Make(Key)

    (* takes: frozen
     * returns: (value, changed, visited) *)
    type 'a result = 'a * bool * S.t
    type 'a exp = S.t -> 'a result

    let pure x _ = (x, false, S.empty)
    let (~) f a frozen =
      let (func, f_changed, f_visited) = f frozen in
      (* note the sideways information passing: in addition to the nodes we see
       * as frozen, `a' sees `f_visited' as frozen.
       * in some sense this is an optimisation. *)
      let (arg, a_changed, a_visited) = a (S.union frozen f_visited) in
      (func arg,
       f_changed || a_changed,
       S.union f_visited a_visited)

    (* my god, what have I done *)
    (* problem: doesn't remember "finished" nodes between calls. hm. *)
    let fix (init: key -> 'v)
            (func: (key -> 'v exp) -> key -> 'v exp)
        : key -> 'v =
      let cache = ref M.empty in
      let put k v = cache := M.add k v (!cache) in
      let get k = try M.find k (!cache)
                  with Not_found -> let v = init k in (put k v; v) in
      let rec visit key frozen : 'v result =
        let cached_value = get key in
        if S.mem key frozen
        (* NB: The only reason we return (S.singleton key) here is so that the
         * check for (not (S.mem key visited)) below, in `loop', works
         * properly. If we removed that we could return S.empty. *)
        then (cached_value, false, S.singleton key)
        else
          let frozen = S.add key frozen in
          let recur () = func visit key frozen in
          let rec loop changed_so_far =
            let (new_value, changed, visited) = recur () in
            let changed = if cached_value = new_value
                          then changed
                          else (put key new_value; true) in
            (* if nothing changed, we're done *)
            if not changed
            then (cached_value, changed_so_far, S.add key visited)
            (* if we didn't depend on ourselves, no need to iterate!
             * TODO: test that this is correct. TEST IT! *)
            else if not (S.mem key visited)
            then (new_value, changed_so_far || changed, S.add key visited)
            (* keep computing until we haven't changed anything. *)
            else loop true
          in loop false
      in fun key -> let (value, _, _) = visit key S.empty in
                    value
end;;

module F = Flow(String)
