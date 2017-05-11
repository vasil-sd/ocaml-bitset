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

module type ComparableType = sig
  type t
  type comp_result = LT | EQ | GT
  val compare : t -> t -> comp_result
end

module ToOrderedType (Ct : ComparableType) : (Set.OrderedType with type t = Ct.t) = struct
  type t = Ct.t
  let compare a b = let open Ct in
                      match compare a b with
                        | LT -> -1
                        | EQ -> 0
                        | GT -> 1
end

module type Universe = sig
  type t
  include ComparableType with type t := t
  val universe : t list
end

module type S_extension = sig
  type t
  val inv : t -> t (* complement to unverse*)
end

(* type of Bitset is compatible with Set.S *)
module type S = sig
  type elt
  type t
  include Set.S with type t := t and type elt := elt
  include S_extension with type t := t
end

module Make (U : Universe) : (S with type elt = U.t) =
  struct
    type elt = U.t
    type t = Bits.bits
    let u = let module OrdType = ToOrderedType(U) in 
               Array.of_list (List.sort_uniq OrdType.compare U.universe)
    exception Not_in_universe of elt
    (* helpers *)
    let check e = let n = ref 0 in
                   if Array.exists (fun el -> U.compare e el == U.EQ || (n:= !n + 1; false)) u
                   then !n 
                   else raise (Not_in_universe e)
    let check_and_copy e s = check e |> fun n -> (Bits.copy s), n
    
    let empty = Bits.make (Array.length u) false
    let is_empty s = Bits.equal empty s
    let mem e s = check e |> Bits.get s
    let add e s = check_and_copy e s |> fun (s, n) -> Bits.set s n true; s
    let singleton e = check_and_copy e empty |> fun (s, n) -> Bits.set s n true; s
    let remove e s = check_and_copy e s |> fun (s, n) -> Bits.set s n false; s
    let union s1 s2 = Bits.(s1 |. s2)
    let inter s1 s2 = Bits.(s1 &. s2)
    let diff s1 s2 = Bits.(let s = copy s2 in (~.! s) &.! s1)
    let compare = Bits.compare
    let equal = Bits.equal
    let subset s1 s2 = equal (inter s1 s2) s1
    let iter f s = Bits.iteri_on_val (fun n -> f u.(n)) s true
    let map f s = let s1 = Bits.copy empty in 
                    Bits.(iteri_on_val (fun n -> check (f u.(n)) |> fun n -> set s1 n true) s true);
                    s1
    let fold f s x = let acc = ref x in iter (fun e -> acc := f e !acc) s; !acc
    let for_all f s = Bits.for_all_values (fun n -> f u.(n)) s true
    let exists f s = Bits.exists_for_values (fun n -> f u.(n)) s true
    let filter f s = Bits.mapi_on_val (fun n -> f u.(n)) s true
    let partition p s = let t = filter p s in
                        let f = Bits.((~. t) &.! s) in
                        (t, f)
    let elements s = List.rev (fold List.cons s [])
    let cardinal s = Bits.count_val s true
    let min_elt s = u.(Bits.index s true)
    let max_elt s = u.(Bits.rindex s true)
    let choose = min_elt
    let find elt s =  check elt |> fun n -> if Bits.get s n then u.(n) else raise Not_found
    let of_list l = let s = Bits.copy empty in List.iter (fun e -> check e |> fun n -> Bits.set s n true) l; s
    let split e s = let open U in
                      filter (fun elt -> compare e elt == GT) s,
                      exists (fun elt -> compare e elt == EQ) s,
                      filter (fun elt -> compare e elt == LT) s

    (* extensions to Set.S iface*)
    let inv s = Bits.bnot s
end
