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

open Format

open Why3
open Pp
open Ident
open Ty
open Term
open Decl
open Printer

let ident_printer =
  let san = sanitizer char_to_alpha char_to_alnumus in
  create_ident_printer [] ~sanitizer:san

let pr_printer =
  let san = sanitizer char_to_lalpha char_to_alnumus in
  create_ident_printer [] ~sanitizer:san

let print_symbol fmt id =
  let san = String.uncapitalize in
  fprintf fmt "%s" (id_unique ~sanitizer:san ident_printer id)

let print_tvar fmt {tv_name = id} =
  let san = String.capitalize in
  fprintf fmt "%s" (id_unique ~sanitizer:san ident_printer id)

let print_var fmt {vs_name = id} =
  let san = String.capitalize in
  fprintf fmt "%s" (id_unique ~sanitizer:san ident_printer id)

let print_pr fmt pr =
  fprintf fmt "%s" (id_unique pr_printer pr.pr_name)

let forget_var v = forget_id ident_printer v.vs_name
let forget_tvar v = forget_id ident_printer v.tv_name

type info = {
  info_syn : syntax_map;
  info_fof : bool;
}

let rec print_type info fmt ty = match ty.ty_node with
  | Tyvar tv -> print_tvar fmt tv
  | Tyapp (ts, tl) -> begin match query_syntax info.info_syn ts.ts_name with
      | Some s -> syntax_arguments s (print_type info) fmt tl
      | None -> begin match tl with
          | [] -> print_symbol fmt ts.ts_name
          | _ -> fprintf fmt "@[%a(%a)@]" print_symbol ts.ts_name
              (print_list comma (print_type info)) tl
          end
      end

let number_format = {
  Number.long_int_support = true;
  Number.extra_leading_zeros_support = false;
  Number.dec_int_support = Number.Number_default;
  Number.hex_int_support = Number.Number_unsupported;
  Number.oct_int_support = Number.Number_unsupported;
  Number.bin_int_support = Number.Number_unsupported;
  Number.def_int_support = Number.Number_unsupported;
  Number.dec_real_support = Number.Number_default;
  Number.hex_real_support = Number.Number_unsupported;
  Number.frac_real_support = Number.Number_custom
    (Number.PrintFracReal ("%s", "(%s * %s)", "(%s / %s)"));
  Number.def_real_support = Number.Number_unsupported
  ;
}

let rec print_app info fmt ls tl oty =
  match query_syntax info.info_syn ls.ls_name with
  | Some s -> syntax_arguments s (print_term info) fmt tl
  | None ->
      let sbs = ls_app_inst ls tl oty in
      if Mtv.is_empty sbs && tl = [] then
        print_symbol fmt ls.ls_name
      else begin
        let cm = if Mtv.is_empty sbs || tl = [] then "" else ", " in
        fprintf fmt "%a(%a%s%a)"
          print_symbol ls.ls_name
          (print_iter2 Mtv.iter comma nothing nothing (print_type info)) sbs
          cm
          (print_list comma (print_term info)) tl
      end

and print_term info fmt t = match t.t_node with
  | Tvar v ->
      print_var fmt v
  | Tapp (ls, tl) ->
      print_app info fmt ls tl t.t_ty
  | Tconst c ->
      Number.print number_format fmt c
  | Tlet (t1,tb) ->
      let v,t2 = t_open_bound tb in
      fprintf fmt "$let_tt(%a@ =@ %a,@ %a)"
        print_symbol v.vs_name (print_term info) t1 (print_term info) t2;
      forget_var v
  | Tif (f1,t1,t2) ->
      fprintf fmt "$ite_t(%a,@ %a,@ %a)"
        (print_fmla info) f1 (print_term info) t1 (print_term info) t2
  | Tcase _ -> unsupportedTerm t
      "TPTP does not support pattern matching, use eliminate_algebraic"
  | Teps _ -> unsupportedTerm t
      "TPTP does not support epsilon-terms"
  | Tquant _ | Tbinop _ | Tnot _ | Ttrue | Tfalse -> raise (TermExpected t)

and print_fmla info fmt f = match f.t_node with
  | Tapp (ls, tl) ->
      print_app info fmt ls tl f.t_ty
  | Tbinop (op, f1, f2) ->
      let s = match op with
        | Tand -> "&" | Tor -> "|" | Timplies -> "=>" | Tiff -> "<=>" in
      fprintf fmt "(%a@ %s %a)" (print_fmla info) f1 s (print_fmla info) f2
  | Tnot f ->
      fprintf fmt "~@ %a" (print_fmla info) f
  | Ttrue ->
      fprintf fmt "$true"
  | Tfalse ->
      fprintf fmt "$false"
  | Tquant (q, fq) ->
      let q = match q with Tforall -> "!" | Texists -> "?" in
      let vl, _tl, f = t_open_quant fq in
      let print_vsty fmt vs =
        if info.info_fof then fprintf fmt "%a" print_var vs
        else fprintf fmt "%a:@,%a" print_var vs (print_type info) vs.vs_ty in
      fprintf fmt "%s[%a]:@ %a" q
        (print_list comma print_vsty) vl (print_fmla info) f;
      List.iter forget_var vl
  | Tlet (t1,tb) ->
      let v,t2 = t_open_bound tb in
      fprintf fmt "$let_tf(%a@ =@ %a,@ %a)"
        print_symbol v.vs_name (print_term info) t1 (print_fmla info) t2;
      forget_var v
  | Tif (f1,t1,t2) ->
      fprintf fmt "$ite_f(%a,@ %a,@ %a)"
        (print_fmla info) f1 (print_fmla info) t1 (print_fmla info) t2
  | Tcase _ -> unsupportedTerm f
      "TPTP does not support pattern matching, use eliminate_algebraic"
  | Tvar _ | Tconst _ | Teps _ -> raise (FmlaExpected f)

let print_tvarg fmt tv = fprintf fmt "%a : $tType" print_tvar tv
let print_tvargs fmt tvs = print_iter1 Stv.iter comma print_tvarg fmt tvs

let star fmt _ = fprintf fmt " *@ "

let print_fmla info fmt f =
  let tvs = t_ty_freevars Stv.empty f in
  if Stv.is_empty tvs then print_fmla info fmt f else
  fprintf fmt "![%a]:@ %a" print_tvargs tvs (print_fmla info) f;
  Stv.iter forget_tvar tvs

let print_decl info fmt d = match d.d_node with
  | Dtype _ when info.info_fof -> ()
  | Dtype { ts_def = Some _ } -> ()
  | Dtype ts when query_syntax info.info_syn ts.ts_name <> None -> ()
  | Dtype ts ->
      let print_arg fmt _ = fprintf fmt "$tType" in
      let print_sig fmt ts = match ts.ts_args with
        | [] -> fprintf fmt "$tType"
        | [_] -> fprintf fmt "$tType >@ $tType"
        | tl -> fprintf fmt "(%a) >@ $tType" (print_list star print_arg) tl
      in
      fprintf fmt "@[<hov 2>tff(%s, type,@ %a:@ %a).@]@\n@\n"
        (id_unique pr_printer ts.ts_name)
        print_symbol ts.ts_name print_sig ts
  | Dparam _ when info.info_fof -> ()
  | Dparam ls when query_syntax info.info_syn ls.ls_name <> None -> ()
  | Dparam ls ->
      let print_type = print_type info in
      let print_val fmt = function
        | Some ty -> print_type fmt ty
        | None -> fprintf fmt "$o" in
      let print_sig fmt ls = match ls.ls_args with
        | [] -> print_val fmt ls.ls_value
        | [ty] -> fprintf fmt "%a >@ %a"
            print_type ty print_val ls.ls_value
        | tl -> fprintf fmt "(%a) >@ %a"
            (print_list star print_type) tl print_val ls.ls_value in
      let print_sig fmt ls =
        let tvs = List.fold_left ty_freevars Stv.empty ls.ls_args in
        let tvs = oty_freevars tvs ls.ls_value in
        if Stv.is_empty tvs then
          print_sig fmt ls
        else if ls.ls_args = [] then
          fprintf fmt "!>[%a]:@ %a" print_tvargs tvs print_sig ls
        else
          fprintf fmt "!>[%a]:@ (%a)" print_tvargs tvs print_sig ls;
        Stv.iter forget_tvar tvs
      in
      fprintf fmt "@[<hov 2>tff(%s, type,@ %a:@ %a).@]@\n@\n"
        (id_unique pr_printer ls.ls_name)
        print_symbol ls.ls_name print_sig ls
  | Ddata _ -> unsupportedDecl d
      "TPTP does not support algebraic datatypes, use eliminate_algebraic"
  | Dlogic _ -> unsupportedDecl d
      "Definitions are not supported, use eliminate_definition"
  | Dind _ -> unsupportedDecl d
      "TPTP does not support inductive predicates, use eliminate_inductive"
  | Dprop (Paxiom, pr, _) when Mid.mem pr.pr_name info.info_syn -> ()
  | Dprop (Paxiom, pr, f) ->
      let head = if info.info_fof then "fof" else "tff" in
      fprintf fmt "@[<hov 2>%s(%a, axiom,@ %a).@]@\n@\n"
        head print_pr pr (print_fmla info) f
  | Dprop (Pgoal, pr, f) ->
      let head = if info.info_fof then "fof" else "tff" in
      fprintf fmt "@[<hov 2>%s(%a, conjecture,@ %a).@]@\n"
        head print_pr pr (print_fmla info) f
  | Dprop ((Plemma|Pskip), _, _) -> assert false

let print_decl info fmt = catch_unsupportedDecl (print_decl info fmt)

let print_task fof _env pr thpr _blacklist ?old:_ fmt task =
  forget_all ident_printer;
  forget_all pr_printer;
  print_prelude fmt pr;
  print_th_prelude task fmt thpr;
  let info = { info_syn = get_syntax_map task; info_fof = fof } in
  fprintf fmt "@[%a@]@."
    (print_list nothing (print_decl info)) (Task.task_decls task)

let () = register_printer "tptp-tff" (print_task false) ~desc:"TPTP TFF format"
let () = register_printer "tptp-fof" (print_task true) ~desc:"TPTP FOF format"



(** DFG input format for SPASS >= 3.8
    (with the help of Daniel Wand)

    TODO:
    - type arguments for polymorphic functions and predicates
    - data types
  *)

let is_type info d = match d.d_node with
  | Dtype ts -> query_syntax info.info_syn ts.ts_name = None
  | Ddata _ -> unsupportedDecl d "no data types"
  | _ -> false
let is_function info d = match d.d_node with
  | Dparam ls ->
      ls.ls_value <> None && query_syntax info.info_syn ls.ls_name = None
  | _ -> false
let is_predicate info d = match d.d_node with
  | Dparam ls ->
      ls.ls_value = None &&  query_syntax info.info_syn ls.ls_name = None
  | _ -> false

let ls_arity fmt d = match d.d_node with
  | Dparam ls ->
      let ntv = Stv.cardinal (ls_ty_freevars ls) in
      let na = List.length ls.ls_args in
      if ntv = 0 then fprintf fmt "(%a, %d)" print_symbol ls.ls_name na
      else fprintf fmt "(%a, %d+%d)" print_symbol ls.ls_name ntv na
  | _ -> assert false
let type_arity fmt d = match d.d_node with
  | Dtype ts ->
      let ntv = List.length ts.ts_args in
      if ntv = 0 then print_symbol fmt ts.ts_name
      else fprintf fmt "(%a, %d)" print_symbol ts.ts_name ntv
  | _ -> assert false

let ls_type kind info fmt d = match d.d_node with
  | Dparam ls ->
      fprintf fmt "%s(@[%a" kind print_symbol ls.ls_name;
      let tvs = ls_ty_freevars ls in
      if not (Stv.is_empty tvs) then
        fprintf fmt ", [%a]" (print_list comma print_tvar) (Stv.elements tvs);
      begin match ls.ls_value, ls.ls_args with
        | None, [] -> ()
        | None, _ ->
            fprintf fmt ", %a" (print_list comma (print_type info)) ls.ls_args
        | Some ty, [] -> fprintf fmt ", %a" (print_type info) ty
        | Some ty, _ ->
            fprintf fmt ", (%a) %a" (print_list comma (print_type info))
              ls.ls_args (print_type info) ty
      end;
      fprintf fmt "@]).@\n";
      Stv.iter forget_tvar tvs
  | _ -> assert false

let rec print_app info fmt ls tl oty =
  match query_syntax info.info_syn ls.ls_name with
  | Some s -> syntax_arguments s (print_term info) fmt tl
  | None ->
      print_symbol fmt ls.ls_name;
      let sbs = ls_app_inst ls tl oty in
      if not (Mtv.is_empty sbs) then
        fprintf fmt "<%a>"
          (print_iter2 Mtv.iter comma nothing nothing (print_type info)) sbs;
      if tl <> [] then
        fprintf fmt "(%a)"
          (print_list comma (print_term info)) tl

and print_term info fmt t = match t.t_node with
  | Tvar v ->
      print_var fmt v
  | Tapp (ls, tl) ->
      print_app info fmt ls tl t.t_ty
  | Tconst c ->
      Number.print number_format fmt c
  | Tlet _ -> unsupportedTerm t
      "DFG does not support let, use eliminate_let"
  | Tif _ -> unsupportedTerm t
      "DFG does not support if, use eliminate_if"
  | Tcase _ -> unsupportedTerm t
      "TPTP does not support pattern matching, use eliminate_algebraic"
  | Teps _ -> unsupportedTerm t
      "TPTP does not support epsilon-terms"
  | Tquant _ | Tbinop _ | Tnot _ | Ttrue | Tfalse -> raise (TermExpected t)

let rec print_fmla info fmt f = match f.t_node with
  | Tapp (ls, tl) ->
      print_app info fmt ls tl f.t_ty
  | Tbinop (op, f1, f2) ->
      let s = match op with
        | Tand -> "and" | Tor -> "or"
        | Timplies -> "implies" | Tiff -> "equiv" in
      fprintf fmt "%s(%a,@ %a)" s (print_fmla info) f1 (print_fmla info) f2
  | Tnot f ->
      fprintf fmt "not(%a)" (print_fmla info) f
  | Ttrue ->
      fprintf fmt "true"
  | Tfalse ->
      fprintf fmt "false"
  | Tquant (q, fq) ->
      let q = match q with Tforall -> "forall" | Texists -> "exists" in
      let vl, _tl, f = t_open_quant fq in
      let print_vsty fmt vs =
        fprintf fmt "%a:@,%a" print_var vs (print_type info) vs.vs_ty in
      fprintf fmt "%s([%a],@ %a)" q
        (print_list comma print_vsty) vl (print_fmla info) f;
      List.iter forget_var vl
  | Tlet _ -> unsupportedTerm f
      "DFG does not support let, use eliminate_let"
  | Tif _ -> unsupportedTerm f
      "DFG does not support if, use eliminate_if"
  | Tcase _ -> unsupportedTerm f
      "DFG does not support pattern matching, use eliminate_algebraic"
  | Tvar _ | Tconst _ | Teps _ -> raise (FmlaExpected f)

let print_axiom info fmt d = match d.d_node with
  | Dprop (Paxiom, pr, _) when Mid.mem pr.pr_name info.info_syn -> ()
  | Dprop (Paxiom, pr, f) ->
      fprintf fmt "@[<hov 2>formula(%a, %a).@]@\n"
        (print_fmla info) f print_pr pr
  | _ -> ()

let print_dfg _env pr thpr _blacklist ?old:_ fmt task =
  forget_all ident_printer;
  forget_all pr_printer;
  fprintf fmt "@[begin_problem(why3).@\n@\n";
  print_prelude fmt pr;
  print_th_prelude task fmt thpr;
  fprintf fmt "list_of_descriptions.@\n";
  fprintf fmt
    "name({**}). author({**}). status(unknown). description({**}).@\n";
  fprintf fmt "end_of_list.@\n@\n";
  let info = { info_syn = get_syntax_map task; info_fof = true } in
  let dl = Task.task_decls task in
  let tl = List.filter (is_type info) dl in
  let fl = List.filter (is_function info) dl in
  let pl = List.filter (is_predicate info) dl in
  (* arities *)
  fprintf fmt "list_of_symbols.@\n";
  fprintf fmt "functions [@[%a@]].@\n" (print_list comma ls_arity) fl;
  fprintf fmt "predicates [@[%a@]].@\n" (print_list comma ls_arity) pl;
  fprintf fmt "sorts [@[%a@]].@\n" (print_list comma type_arity) tl;
  fprintf fmt "end_of_list.@\n@\n";
  (* types *)
  fprintf fmt "list_of_declarations.@\n";
  List.iter (ls_type "function" info fmt) fl;
  List.iter (ls_type "predicate" info fmt) pl;
  fprintf fmt "end_of_list.@\n@\n";

  fprintf fmt "list_of_formulae(axioms).@\n";
  List.iter (print_axiom info fmt) dl;
  fprintf fmt "end_of_list.@\n@\n";
  fprintf fmt "list_of_formulae(conjectures).@\n";

  let f = Task.task_goal_fmla task in
  fprintf fmt "@[<hov 2>formula(%a, %a)@].@\n" (print_fmla info) f
    print_pr (Task.task_goal task);
  fprintf fmt "end_of_list.@\n@\n";
  fprintf fmt "end_problem.@\n"

let () =
  register_printer "dfg" print_dfg ~desc:"First-order monomorphic DFG format"
