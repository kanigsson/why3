(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-                                                   *)
(*    Francois Bobot                                                      *)
(*    Jean-Christophe Filliatre                                           *)
(*    Johannes Kanig                                                      *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Identifiers *)

type ident = {
  id_short : string;    (* non-unique name for string-based lookup *)
  id_long : string;     (* non-unique name for pretty printing *)
  id_origin : origin;   (* origin of the ident *)
  id_tag : int;         (* unique numeric tag *)
}

and origin =
  | User of Loc.position
  | Derived of ident
  | Fresh

module Id = struct
  type t = ident
  let equal = (==)
  let hash id1 = id1.id_tag
  let compare id1 id2 = Pervasives.compare id1.id_tag id2.id_tag
end
module Mid = Map.Make(Id)
module Sid = Set.Make(Id)
module Hid = Hashtbl.Make(Id)

type preid = ident

(* constructors *)

let gentag = let r = ref 0 in fun () -> incr r; !r

let id_register id = { id with id_tag = gentag () }

let create_ident short long origin = {
  id_short  = short;
  id_long   = long;
  id_origin = origin;
  id_tag    = -1
}

let id_fresh sh = create_ident sh sh Fresh
let id_fresh_long sh ln = create_ident sh ln Fresh

let id_user sh loc = create_ident sh sh (User loc)
let id_user_long sh ln loc = create_ident sh ln (User loc)

let id_derive sh id = create_ident sh sh (Derived id)
let id_derive_long sh ln id = create_ident sh ln (Derived id)

let id_clone id = create_ident id.id_short id.id_long (Derived id)
let id_dup id = { id with id_tag = -1 }

(** Unique names for pretty printing *)

type printer = (string, int) Hashtbl.t * (int, string) Hashtbl.t

let create_printer () = Hashtbl.create 1997, Hashtbl.create 1997

let rec find_index indices name ind =
  if Hashtbl.mem indices (name ^ string_of_int ind)
  then find_index indices name (succ ind) else ind

let find_unique indices name =
  try
    let ind = Hashtbl.find indices name + 1 in
    let ind = find_index indices name ind in
    Hashtbl.add indices name ind;
    name ^ string_of_int ind
  with Not_found ->
    name

let id_unique (indices,values) id =
  try
    Hashtbl.find values id.id_tag
  with Not_found ->
    let name = find_unique indices id.id_long in
    Hashtbl.add values id.id_tag name;
    Hashtbl.add indices name 0;
    name

