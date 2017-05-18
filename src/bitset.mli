(*---------------------------------------------------------------------------
  Copyright (c) 2017 Vasil Diadov. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.
  %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Library for manipulating sets from defined universe.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bitset} *)

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
  val inv : t -> t (* complement to unverse *)
  val extend_universe : elt list -> unit
end

(** type Bitset.S is compatible with Set.S *)
module type S = sig
  type elt
  type t
  include Set.S with type t := t and type elt := elt
  include S_EXTENSION with type t := t and type elt := elt
end

module Make (U : UNIVERSE) : (S with type elt = U.t)
