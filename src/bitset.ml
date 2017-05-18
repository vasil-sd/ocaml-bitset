(*---------------------------------------------------------------------------
  Copyright (c) 2017 Vasil Diadov. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.
  %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
  Copyright (c) 2017 Vasil Diadov

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

module type UNIVERSE = sig
  type t
  val universe : t list
  val compare : t -> t -> int
end

module type S_EXTENSION = sig
  type elt
  type t
  val inv : t -> t
  val extend_universe : elt list -> unit
end

module type S = sig
  type elt
  type t
  include Set.S with type t := t and type elt := elt
  include S_EXTENSION with type t := t and type elt := elt
end

module Make (U : UNIVERSE) : (S with type elt = U.t) = struct
  type elt = U.t
  type t = Bits.bits ref

  (** Universe *)
  let u = ref (Array.of_list (List.sort_uniq U.compare U.universe))

  exception Not_in_universe of elt
  exception Set_is_larger_than_universe

  (** Helpers *)

  (** Check if set is not extended and extend it if neccessary *)

  let maybe_extend s =
    let s_len = Bits.length !s in
    let u_len = Array.length !u in
    if s_len <= u_len
    then
      if s_len = u_len
      then s
      else ref (Bits.extend !s 0 (u_len - s_len))
    else
      raise Set_is_larger_than_universe

  let ( ~<> ) s = maybe_extend s

  (** Check if element in the universe and return its index *)
  let check e =
    let n = ref 0 in
    if Array.exists
         (fun el -> U.compare e el == 0 || (incr n; false))
         !u
    then !n
    else raise (Not_in_universe e)

  (** Check existance of element e and make copy of bitset s *)
  let check_and_copy e s =
    let copy_and_return n = ref (Bits.copy !(~<>s)), n in
    check e |> copy_and_return

  let set_bit bits index = Bits.set !bits index true; bits
  let set_bit_imp bits index = Bits.set !bits index true
  let clear_bit bits index = Bits.set !bits index false; bits
  let ( ||> ) x f = let a, b = x in f a b

  let make_empty () = Bits.make (Array.length !u) false
  let empty = ref (make_empty ())
  let is_empty s = Bits.all_zeros !s
  let mem e s = check e |> Bits.get !s
  let add e s = check_and_copy e s ||> set_bit
  let singleton e = check_and_copy e (ref (make_empty ())) ||> set_bit
  let remove e s = check_and_copy e s ||> clear_bit
  let union s1 s2 = ref Bits.(!(~<>s1) lor !s2)
  let inter s1 s2 = ref Bits.(!(~<>s1) land !s2)
  let diff s1 s2 = ref Bits.(land_inplace (lnot !(~<>s2)) !s1)
  let compare s1 s2 = Bits.compare !s1 !s2
  let equal s1 s2 = Bits.equal !s1 !s2
  let subset s1 s2 = Bits.subset !s1 !s2
  let iter f s = Bits.iteri_on_val (fun n -> f !u.(n)) !s true
  let map f s =
    let s1 = ref (make_empty ()) in
    Bits.(iteri_on_val (fun n -> check (f !u.(n)) |> set_bit_imp s1) !s true);
    s1
  let fold f s x = let acc = ref x in iter (fun e -> acc := f e !acc) s; !acc
  let for_all f s = Bits.for_all_values (fun n -> f !u.(n)) !s true
  let exists f s = Bits.exists_for_values (fun n -> f !u.(n)) !s true
  let filter f s = ref (Bits.mapi_on_val (fun n -> f !u.(n)) !(~<>s) true)
  let partition p s =
    let t = filter p s in
    let f = ref Bits.(land_inplace (lnot !t) !s) in
    (t, f)
  let elements s = List.rev (fold List.cons s [])
  let cardinal s = Bits.count_val !s true
  let min_elt s = !u.(Bits.index !s true)
  let max_elt s = !u.(Bits.rindex !s true)
  let choose = min_elt
  let find elt s =
    let check_bit_for_elt n = if Bits.get !s n then !u.(n) else raise Not_found in
    check elt |> check_bit_for_elt
  let of_list l =
    let s = ref (make_empty ()) in
    List.iter (fun e -> check e |> set_bit_imp s) l;
    s
  let split e s =
    let open U in
    filter (fun elt -> compare e elt == 1) s,
    exists (fun elt -> compare e elt == 0) s,
    filter (fun elt -> compare e elt == -1) s

  (* extensions to Set.S iface*)
  let inv s = ref (Bits.lnot !(~<>s))
  let extend_universe l =
    let is_element_new e = not (Array.exists (fun el -> U.compare e el == 0) !u) in
    u := Array.append
           !u
           (Array.of_list
              (List.sort_uniq
                 U.compare
                 (List.filter is_element_new l)));
    empty := make_empty ()
end
