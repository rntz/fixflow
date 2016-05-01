(* not thread-safe. *)
module type DEBUG = sig
    val indent : int -> unit
    val dedent : int -> unit
    val with_indent : int -> (unit -> 'a) -> 'a

    val current_indent : unit -> int
    val indent_string : unit -> string
    val print_endline : string -> unit
    val println : ('a, out_channel, unit) format -> 'a
end

module Debug : DEBUG = struct
    let the_indent = ref 0
    let indent i = (assert (i >= 0);
                    the_indent := !the_indent + i)
    let dedent i = (assert (i >= 0);
                    assert (i <= !the_indent);
                    the_indent := !the_indent - i)

    let with_indent i f =
      let old = !the_indent in
      indent i;
      let result = try f () with exn -> (the_indent := old; raise exn) in
      dedent i;
      assert (!the_indent = old);
      result

    let current_indent () = !the_indent
    let indent_string () = String.make !the_indent ' '
    let print_endline s = Pervasives.print_endline (indent_string () ^ s)
    let println fmt = Printf.printf
                        (Scanf.format_from_string (indent_string ()) "" ^^ fmt)

end
