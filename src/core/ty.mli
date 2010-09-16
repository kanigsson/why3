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

open Ident

(** Types *)

type tvsymbol = private {
  tv_name : ident;
}

module Stv : Set.S with type elt = tvsymbol
module Mtv : Map.S with type key = tvsymbol
module Htv : Hashtbl.S with type key = tvsymbol

val tv_equal : tvsymbol -> tvsymbol -> bool

val tv_hash : tvsymbol -> int

val create_tvsymbol : preid -> tvsymbol

(* type symbols and types *)

type tysymbol = private {
  ts_name : ident;
  ts_args : tvsymbol list;
  ts_def  : ty option;
}

and ty = private {
  ty_node : ty_node;
  ty_tag  : Hashweak.tag;
}

and ty_node = private
  | Tyvar of tvsymbol
  | Tyapp of tysymbol * ty list

module Sts : Set.S with type elt = tysymbol
module Mts : Map.S with type key = tysymbol
module Hts : Hashtbl.S with type key = tysymbol
module Wts : Hashweak.S with type key = tysymbol

module Sty : Set.S with type elt = ty
module Mty : Map.S with type key = ty
module Hty : Hashtbl.S with type key = ty
module Wty : Hashweak.S with type key = ty

val ts_equal : tysymbol -> tysymbol -> bool
val ty_equal : ty -> ty -> bool

val ts_hash : tysymbol -> int
val ty_hash : ty -> int

exception BadTypeArity of tysymbol * int * int
exception DuplicateTypeVar of tvsymbol
exception UnboundTypeVar of tvsymbol

val create_tysymbol : preid -> tvsymbol list -> ty option -> tysymbol

val ty_var : tvsymbol -> ty
val ty_app : tysymbol -> ty list -> ty

val ty_map : (ty -> ty) -> ty -> ty
val ty_fold : ('a -> ty -> 'a) -> 'a -> ty -> 'a
val ty_all : (ty -> bool) -> ty -> bool
val ty_any : (ty -> bool) -> ty -> bool

val ty_s_map : (tysymbol -> tysymbol) -> ty -> ty
val ty_s_fold : ('a -> tysymbol -> 'a) -> 'a -> ty -> 'a
val ty_s_all : (tysymbol -> bool) -> ty -> bool
val ty_s_any : (tysymbol -> bool) -> ty -> bool

exception TypeMismatch of ty * ty

val ty_match : ty Mtv.t -> ty -> ty -> ty Mtv.t
val ty_inst  : ty Mtv.t -> ty -> ty

val ty_freevars : Stv.t -> ty -> Stv.t

(* built-in symbols *)

val ts_int  : tysymbol
val ts_real : tysymbol

val ty_int  : ty
val ty_real : ty

val ts_func : tysymbol
val ts_pred : tysymbol

val ty_func : ty -> ty -> ty
val ty_pred : ty -> ty

val ts_tuple : int -> tysymbol
val ty_tuple : ty list -> ty

val is_ts_tuple : tysymbol -> bool

