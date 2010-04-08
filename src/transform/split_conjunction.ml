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
open Term
open Decl

let list_fold_product f acc l1 l2 =
  List.fold_left 
    (fun acc e1 ->
       List.fold_left 
         (fun acc e2 -> f acc e1 e2) 
         acc l2) 
    acc l1

let rec split_pos split_neg acc f =
  let split_pos acc f = 
    let p = split_pos split_neg acc f in
    (*Format.printf "@[<hov 2>f : %a@\n acc : %a :@\n %a@]@." 
      Pretty.print_fmla f
      (Pp.print_list Pp.semi Pretty.print_fmla) acc
      (Pp.print_list Pp.semi Pretty.print_fmla) p;*)
    p in

  match f.f_node with
    | Ftrue -> acc
    | Ffalse -> f::acc
    | Fapp _ -> f::acc
    | Fbinop (Fand,f1,f2) -> split_pos (split_pos acc f2) f1
    | Fbinop (Fimplies,f1,f2) -> 
        list_fold_product 
          (fun acc f1 f2 ->  (f_implies f1 f2)::acc) 
          acc (split_neg [] f1) (split_pos [] f2)
    | Fbinop (Fiff,f1,f2) -> 
        split_pos (split_pos acc (f_implies f1 f2)) (f_implies f2 f1)
    | Fbinop (For,f1,f2) -> 
        list_fold_product 
          (fun acc f1 f2 ->  (f_or f1 f2)::acc) 
          acc (split_pos [] f1) (split_pos [] f2)
    | Fnot f -> List.fold_left (fun acc f -> f_not f::acc) acc (split_neg [] f)
    | Fif (fif,fthen,felse) -> 
        split_pos 
          (split_pos acc (f_implies fif fthen)) 
          (f_implies (f_not fif) felse)
    | Flet (t,fb) ->
        let vs,f = f_open_bound fb in
        List.fold_left (fun acc f -> (f_let vs t f)::acc) acc (split_pos [] f)
    | Fcase _ -> (* TODO better *) f::acc
    | Fquant (Fforall,fmlaq) ->
        let vsl,trl,fmla = f_open_quant fmlaq in
        List.fold_left (fun acc f -> 
                          (* TODO : Remove unused variable*)
                          (f_forall vsl trl f)::acc) acc (split_pos [] fmla)
    | Fquant (Fexists,_) -> f::acc

let rec split_neg split_pos acc f =
  let split_neg = split_neg split_pos in
  match f.f_node with
    | Ftrue -> f::acc
    | Ffalse -> acc
    | Fapp _ -> f::acc
    | Fbinop (Fand,f1,f2) -> 
        list_fold_product 
          (fun acc f1 f2 ->  (f_and f1 f2)::acc) 
          acc (split_neg [] f1) (split_neg [] f2)
    | Fbinop (Fimplies,f1,f2) -> split_pos (split_neg acc f2) (f_not f1)
    | Fbinop (Fiff,f1,f2) -> 
        split_neg (split_neg acc (f_and (f_not f1) (f_not f2))) (f_and f2 f1)
    | Fbinop (For,f1,f2) -> split_neg (split_neg acc f2) f1
    | Fnot f -> List.fold_left (fun acc f -> f_not f::acc) acc (split_pos [] f)
    | Fif (fif,fthen,felse) -> 
        split_neg (split_neg acc (f_and fif fthen))
          (f_and (f_not fif) felse)
    | Flet (t,fb) ->
        let vs,f = f_open_bound fb in
        List.fold_left (fun acc f -> (f_let vs t f)::acc) acc (split_neg [] f)
    | Fcase _ -> (* TODO better *) f::acc
    | Fquant (Fexists,fmlaq) ->
        let vsl,trl,fmla = f_open_quant fmlaq in
        List.fold_left (fun acc f -> 
                          (* TODO : Remove unused variable*)
                          (f_exists vsl trl f)::acc) acc (split_neg [] fmla)
    | Fquant (Fforall,_) -> f::acc


let elt is_split_kind split_pos d =
  match d.d_node with
    | Dprop (k,pr,f) when is_split_kind k ->
        let l = split_pos [] f in
        let l = List.map (fun p -> create_prop_decl k 
                            (create_prsymbol (id_clone pr.pr_name)) p) l in
        begin match k with
          | Pgoal -> List.map (fun p -> [p]) l
          | Paxiom -> [l]
          | _ -> assert false
        end
    | _ -> [[d]]

let is_split_goal = function Pgoal -> true | _ -> false
let is_split_axiom = function Paxiom -> true | _ -> false
let is_split_all = function _ -> true

let t isk fsp = Register.store (fun () -> Trans.decl_l (elt isk fsp) None)

let split_pos1 = split_pos (fun acc x -> x::acc)

let rec split_pos2 acc d = split_pos split_neg2 acc d
and split_neg2 acc d = split_neg split_pos2 acc d

let split_pos_goal = t is_split_goal split_pos1
let split_pos_neg_goal = t is_split_goal split_pos2
let split_pos_axiom = t is_split_axiom split_pos1
let split_pos_neg_axiom = t is_split_axiom split_pos2
let split_pos_all = t is_split_all split_pos1
let split_pos_neg_all = t is_split_all split_pos2

let () = Driver.register_transform_l 
  "split_goal_pos_goal" split_pos_goal
let () = Driver.register_transform_l 
  "split_goal_pos_neg_goal" split_pos_neg_goal
let () = Driver.register_transform_l 
  "split_goal_pos_axiom" split_pos_axiom
let () = Driver.register_transform_l 
  "split_goal_pos_neg_axiom" split_pos_neg_axiom
let () = Driver.register_transform_l 
  "split_goal_pos_all" split_pos_all
let () = Driver.register_transform_l 
  "split_goal_pos_neg_all" split_pos_neg_all
