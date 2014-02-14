(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Ident
open Ty
open Term
open Decl

(** Discard definitions of built-in symbols *)

let add_id undef_ls rem_ls (ls,ld) (abst,defn) =
  if Sls.mem ls rem_ls then
    abst,defn
  else if Sls.mem ls undef_ls
  then create_param_decl ls :: abst, defn
  else abst, (ls,ld) :: defn

(** TODO: go further? such as constructor that are removed? *)

let elim_abstract undef_ls rem_pr rem_ls rem_ts d = match d.d_node with
  | Dlogic l ->
      let ld, id = List.fold_right (add_id undef_ls rem_ls) l ([],[]) in
      ld @ (if id = [] then [] else [create_logic_decl id])
  | Dind (s, l) ->
      let ld, id = List.fold_right (add_id undef_ls rem_ls) l ([],[]) in
      ld @ (if id = [] then [] else [create_ind_decl s id])
  | Dprop (Paxiom,pr,_) when Spr.mem pr rem_pr -> []
  | Dtype ts when Sts.mem ts rem_ts -> []
  | Ddata l ->
    let test_id (ts,_) = not (Sts.mem ts rem_ts) in
    let l = List.filter test_id l in
    (if l = [] then [] else [create_data_decl l])
  | _ -> [d]

let eliminate_builtin =
  Trans.on_tagged_ls Printer.meta_syntax_logic (fun undef_ls ->
  Trans.on_tagged_pr Printer.meta_remove_prop (fun rem_pr ->
  Trans.on_tagged_ls Printer.meta_remove_logic (fun rem_ls ->
  Trans.on_tagged_ts Printer.meta_remove_type (fun rem_ts ->
    Trans.decl (elim_abstract undef_ls rem_pr rem_ls rem_ts) None))))

let () = Trans.register_transform "eliminate_builtin" eliminate_builtin
  ~desc:"Eliminate@ propositions@ and@ definitions@ of@ symbols@ \
    that@ are@ builtin@ in@ the@ prover@ (see@ 'syntax'@ and@ \
    'remove'@ clauses@ in@ the@ prover's@ driver)."

(** compute the meta_remove_* given two task one included in the other *)
let compute_diff t1 t2 =
  let km = Mid.set_diff (Task.task_known t1) (Task.task_known t2) in
  let hdone = Hdecl.create 10 in
  let remove_ts acc ts =
    (Printer.meta_remove_type, [Theory.MAts ts])::acc in
  let remove_ls acc ls =
    (Printer.meta_remove_logic, [Theory.MAls ls])::acc in
  let remove_pr acc pr =
    (Printer.meta_remove_prop, [Theory.MApr pr])::acc in
  Mid.fold_left (fun acc _ decl ->
    if Hdecl.mem hdone decl then acc
    else begin
      Hdecl.replace hdone decl ();
      match decl.d_node with
      | Dtype ts -> remove_ts acc ts
      | Ddata l -> List.fold_left (fun acc (ts,_) -> remove_ts acc ts) acc l
      | Dparam ls -> remove_ls acc ls
      | Dlogic l -> List.fold_left (fun acc (ls,_) -> remove_ls acc ls) acc l
      | Dind (_,l) -> List.fold_left (fun acc (ls,_) -> remove_ls acc ls) acc l
      | Dprop (_,pr,_) -> remove_pr acc pr
    end) [] km

let compute_diff =
  Trans.store (fun t1 -> Trans.store (fun t2 -> compute_diff t1 t2))

(** Eliminate definitions of functions and predicates *)

let rec t_insert hd t = match t.t_node with
  | Tif (f1,t2,t3) ->
      t_if f1 (t_insert hd t2) (t_insert hd t3)
  | Tlet (t1,bt) ->
      let v,t2 = t_open_bound bt in
      t_let_close v t1 (t_insert hd t2)
  | Tcase (tl,bl) ->
      let br b =
        let pl,t1 = t_open_branch b in
        t_close_branch pl (t_insert hd t1)
      in
      t_case tl (List.map br bl)
  | _ -> TermTF.t_selecti t_equ_simp t_iff_simp hd t

let add_ld which (ls,ld) (abst,defn,axl) =
  if which ls then
    let vl,e = open_ls_defn ld in
    let nm = ls.ls_name.id_string ^ "_def" in
    let pr = create_prsymbol (id_derive nm ls.ls_name) in
    let hd = t_app ls (List.map t_var vl) e.t_ty in
    let ax = t_forall_close vl [[hd]] (t_insert hd e) in
    let ax = create_prop_decl Paxiom pr ax in
    let ld = create_param_decl ls in
    ld :: abst, defn, ax :: axl
  else
    abst, (ls,ld) :: defn, axl

let elim_decl which l =
  let abst,defn,axl = List.fold_right (add_ld which) l ([],[],[]) in
  let defn = if defn = [] then [] else [create_logic_decl defn] in
  abst @ defn @ axl

let elim which d = match d.d_node with
  | Dlogic l -> elim_decl which l
  | _ -> [d]

let elim_recursion d = match d.d_node with
  | Dlogic ([s,_] as l)
    when Sid.mem s.ls_name d.d_syms -> elim_decl Util.ttrue l
  | Dlogic l when List.length l > 1 -> elim_decl Util.ttrue l
  | _ -> [d]

let is_struct dl = (* FIXME? Shouldn't 0 be allowed too? *)
  List.for_all (fun (_,ld) -> List.length (ls_defn_decrease ld) = 1) dl

(* FIXME? We can have non-recursive functions in a group *)
let elim_non_struct_recursion d = match d.d_node with
  | Dlogic ((s,_) :: _ as dl)
    when Sid.mem s.ls_name d.d_syms && not (is_struct dl) ->
      elim_decl Util.ttrue dl
  | _ ->
      [d]

let elim_mutual d = match d.d_node with
  | Dlogic l when List.length l > 1 -> elim_decl Util.ttrue l
  | _ -> [d]

let eliminate_definition_gen which = Trans.decl (elim which) None

let eliminate_definition_func  =
  eliminate_definition_gen (fun ls -> ls.ls_value <> None)
let eliminate_definition_pred  =
  eliminate_definition_gen (fun ls -> ls.ls_value =  None)
let eliminate_definition       =
  eliminate_definition_gen Util.ttrue

let eliminate_recursion        = Trans.decl elim_recursion None
let eliminate_non_struct_recursion = Trans.decl elim_non_struct_recursion None
let eliminate_mutual_recursion = Trans.decl elim_mutual None

let () =
  Trans.register_transform "eliminate_definition_func"
    eliminate_definition_func
    ~desc:"Transform@ function@ definitions@ into@ axioms.";
  Trans.register_transform "eliminate_definition_pred"
    eliminate_definition_pred
    ~desc:"Transform@ predicate@ definitions@ into@ axioms.";
  Trans.register_transform "eliminate_definition"
    eliminate_definition
    ~desc:"Transform@ function@ and@ predicate@ definitions@ into@ axioms.";
  Trans.register_transform "eliminate_recursion"
    eliminate_recursion
    ~desc:"Same@ as@ eliminate_definition,@ but@ only@ for@ recursive@ \
           definitions.";
  Trans.register_transform "eliminate_non_struct_recursion"
    eliminate_non_struct_recursion
    ~desc:"Same@ as@ eliminate_recursion,@ but@ only@ for@ non-structural@ \
           recursive@ definitions.";
  Trans.register_transform "eliminate_mutual_recursion"
    eliminate_mutual_recursion
    ~desc:"Same@ as@ eliminate_recursion,@ but@ only@ for@ mutually@ \
           recursive@ definitions."

(** Bisect *)
open Task
open Theory

type bisect_step =
 | BSdone of (Theory.meta * Theory.meta_arg list) list
 | BSstep of task * (bool -> bisect_step)

type rem = { rem_pr : Spr.t; rem_ls : Sls.t; rem_ts : Sts.t }

let _print_rem fmt rem = Format.fprintf fmt
  "@[rem_pr:@[%a@]@\nrem_ls:@[%a@]@\nrem_ts:@[%a@]@\n"
  (Pp.print_iter1 Spr.iter Pp.comma Pretty.print_pr) rem.rem_pr
  (Pp.print_iter1 Sls.iter Pp.comma Pretty.print_ls) rem.rem_ls
  (Pp.print_iter1 Sts.iter Pp.comma Pretty.print_ts) rem.rem_ts

let rec elim_task task rem =
  match task with
  | Some ({task_decl = {td_node = Decl decl}} as task) ->
    let task = elim_task task.task_prev rem in
    let l = elim_abstract Sls.empty
      rem.rem_pr rem.rem_ls rem.rem_ts decl in
    List.fold_left Task.add_decl task l
  | Some task ->
    Task.add_tdecl (elim_task task.task_prev rem) task.task_decl
  | None      -> None


let add_rem rem decl =
  let remove_ts rem ts =
    { rem with rem_ts = Sts.add ts rem.rem_ts} in
  let remove_ls rem ls =
    { rem with rem_ls = Sls.add ls rem.rem_ls} in
  let remove_pr rem pr =
    { rem with rem_pr = Spr.add pr rem.rem_pr} in
  match decl.d_node with
  | Dtype ts -> remove_ts rem ts
  | Ddata l -> List.fold_left (fun rem (ts,_) -> remove_ts rem ts) rem l
  | Dparam ls -> remove_ls rem ls
  | Dlogic l -> List.fold_left (fun rem (ls,_) -> remove_ls rem ls) rem l
  | Dind (_,l) -> List.fold_left (fun rem (ls,_) -> remove_ls rem ls) rem l
  | Dprop (_,pr,_) -> remove_pr rem pr

let _union_rem rem1 rem2 =
  { rem_ts = Sts.union rem1.rem_ts rem2.rem_ts;
    rem_ls = Sls.union rem1.rem_ls rem2.rem_ls;
    rem_pr = Spr.union rem1.rem_pr rem2.rem_pr;
  }

let create_meta_rem_list rem =
  let remove_ts acc ts =
    (Printer.meta_remove_type, [Theory.MAts ts])::acc in
  let remove_ls acc ls =
    (Printer.meta_remove_logic, [Theory.MAls ls])::acc in
  let remove_pr acc pr =
    (Printer.meta_remove_prop, [Theory.MApr pr])::acc in
  let acc = Sts.fold_left remove_ts [] rem.rem_ts in
  let acc = Sls.fold_left remove_ls acc rem.rem_ls in
  let acc = Spr.fold_left remove_pr acc rem.rem_pr in
  acc

let fold_sub f acc a i1 i2 =
  let acc = ref acc in
  for i=i1 to i2-1 do
    acc := f !acc a.(i)
  done;
  !acc

let rec bisect_aux task a i1 i2 rem cont       (* lt i lk *) =
  (* Format.eprintf "i1: %i, i2: %i@\nrem:%a@." i1 i2 *)
  (*   print_rem rem; *)
  let call rem valid invalid =
    try BSstep (elim_task task rem,
                fun b -> if b then valid () else invalid ())
    with UnknownIdent _ -> invalid ()
  in
  if i2 - i1 < 2 then
    let rem1 = add_rem rem a.(i1) in
    call rem1
      (fun () -> assert (i2 - i1 = 1); cont rem1)
      (fun () -> cont rem)
  else
    let m = (i1+i2)/2 in
    let rem1 = fold_sub add_rem rem a m i2 in
    call rem1
      (fun () -> bisect_aux task a i1 m rem1 cont)
      (fun () ->
        bisect_aux task a m i2 rem
          (fun rem1 -> (* rem c rem1 c \old(rem1) *)
            let rem2 = fold_sub add_rem rem1 a i1 m in
            call rem2
              (fun () -> cont rem2)
              (fun () -> bisect_aux task a i1 m rem1 cont)))

let bisect_step task0 =
  let task= match task0 with
    | Some {task_decl = {td_node = Decl {d_node = Dprop (Pgoal,_,_)}};
            task_prev = task} -> task
    | _ -> raise GoalNotFound in
  let rec length acc = function
    | Some {task_decl = {td_node = Decl _};
            task_prev = t} -> length (acc + 1) t
    | Some {task_prev = t} -> length acc t
    | None -> acc in
  let n = length 0 task in
  let a = Array.create n (Obj.magic 0) in
  let rec init acc = function
    | Some {task_decl = {td_node = Decl d}; task_prev = t} ->
      a.(acc) <- d; init (acc - 1) t
    | Some { task_prev = t} -> init acc t
    | None -> assert (acc = -1) in
  init (n-1) task;
  let empty_rem = {rem_ts = Sts.empty; rem_ls = Sls.empty;
                   rem_pr = Spr.empty} in
  bisect_aux task0 a 0 n empty_rem
    (fun rem -> BSdone (create_meta_rem_list rem))

let bisect f task =
  let rec run = function
    | BSdone r -> r
    | BSstep (t,c) -> run (c (f t)) in
  run (bisect_step task)

(** catch exception for debug *)
(* let bisect_step task0 = *)
(*   let res = try bisect_step task0 with exn -> *)
(*     Format.eprintf "bisect_step fail: %a@." Exn_printer.exn_printer exn; *)
(*     raise exn in *)
(*   match res with *)
(*   | BSdone _ as d -> d *)
(*   | BSstep (t,f) -> BSstep (t,fun b -> try f b with exn -> *)
(*     Format.eprintf "bisect_step fail: %a@." Exn_printer.exn_printer exn; *)
(*     raise exn) *)
