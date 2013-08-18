(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: location.ml 12511 2012-05-30 13:29:48Z lefessan $ *)

open Misc
open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

(* Highlight the locations using standout mode. *)

let highlight_terminfo ppf num_lines lb loc1 loc2 =
  Format.pp_print_flush ppf ();  (* avoid mixing Format and normal output *)
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if lb.lex_buffer.[i] = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= num_lines - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if pos = loc1.loc_start.pos_cnum || pos = loc2.loc_start.pos_cnum then
      Terminfo.standout true;
    if pos = loc1.loc_end.pos_cnum || pos = loc2.loc_end.pos_cnum then
      Terminfo.standout false;
    let c = lb.lex_buffer.[pos + pos0] in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout false;
  (* Position cursor back to original location *)
  Terminfo.resume !num_loc_lines;
  flush stdout

(* Highlight the location by printing it again. *)

let highlight_dumb ppf lb loc =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  let end_pos = lb.lex_buffer_len - pos0 - 1 in
  (* Determine line numbers for the start and end points *)
  let line_start = ref 0 and line_end = ref 0 in
  for pos = 0 to end_pos do
    if lb.lex_buffer.[pos + pos0] = '\n' then begin
      if loc.loc_start.pos_cnum > pos then incr line_start;
      if loc.loc_end.pos_cnum   > pos then incr line_end;
    end
  done;
  (* Print character location (useful for Emacs) *)
  Format.fprintf ppf "Characters %i-%i:@."
                 loc.loc_start.pos_cnum loc.loc_end.pos_cnum;
  (* Print the input, underlining the location *)
  Format.pp_print_string ppf "  ";
  let line = ref 0 in
  let pos_at_bol = ref 0 in
  for pos = 0 to end_pos do
    let c = lb.lex_buffer.[pos + pos0] in
    if c <> '\n' then begin
      if !line = !line_start && !line = !line_end then
        (* loc is on one line: print whole line *)
        Format.pp_print_char ppf c
      else if !line = !line_start then
        (* first line of multiline loc: print ... before loc_start *)
        if pos < loc.loc_start.pos_cnum
        then Format.pp_print_char ppf '.'
        else Format.pp_print_char ppf c
      else if !line = !line_end then
        (* last line of multiline loc: print ... after loc_end *)
        if pos < loc.loc_end.pos_cnum
        then Format.pp_print_char ppf c
        else Format.pp_print_char ppf '.'
      else if !line > !line_start && !line < !line_end then
        (* intermediate line of multiline loc: print whole line *)
        Format.pp_print_char ppf c
    end else begin
      if !line = !line_start && !line = !line_end then begin
        (* loc is on one line: underline location *)
        Format.fprintf ppf "@.  ";
        for i = !pos_at_bol to loc.loc_start.pos_cnum - 1 do
          Format.pp_print_char ppf ' '
        done;
        for i = loc.loc_start.pos_cnum to loc.loc_end.pos_cnum - 1 do
          Format.pp_print_char ppf '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        Format.fprintf ppf "@.";
        if pos < loc.loc_end.pos_cnum then Format.pp_print_string ppf "  "
      end;
      incr line;
      pos_at_bol := pos + 1;
    end
  done

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf loc1 loc2 =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf loc1 loc2
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            try highlight_dumb ppf lb loc1; true
            with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf num_lines lb loc1 loc2; true
          with Exit -> false
      end

(* Print the location in some way or another *)

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_line, msg_chars, msg_to, msg_colon) =
  ("Line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let print_loc ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
    if highlight_locations ppf loc none then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s%i" msg_line line;
    if startchar >= 0 then
      fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar
  end
;;

let print ppf loc =
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf loc none then ()
  else fprintf ppf "%a%s@." print_loc loc msg_colon
;;

let print_error ppf loc =
  print ppf loc;
  fprintf ppf "Error: ";
;;

let print_error_cur_file ppf = print_error ppf (in_file !input_name);;

let print_warning loc ppf w =
  if Warnings.is_active w then begin
    let printw ppf w =
      let n = Warnings.print ppf w in
      num_loc_lines := !num_loc_lines + n
    in
    fprintf ppf "Warning %a@." printw w;
    pp_print_flush ppf ();
    incr num_loc_lines;
  end
;;

exception Warning of t * string

let warnings : exn list ref option fluid = fluid None

let raise_warning exn =
  match ~!warnings with
  | None -> raise exn
  | Some l -> l := exn :: !l

let prerr_warning loc w =
  match ~!warnings with
  | None -> print_warning loc err_formatter w
  | Some l ->
    let ppf, to_string = Misc.ppf_to_string () in
    print_warning loc ppf w;
    match to_string () with
      | "" -> ()
      | s ->  l := Warning (loc,s) :: !l

let catch_warnings f =
  let caught = ref [] in
  let result = 
    try_sum (fun () -> fluid'let warnings (Some caught) f)
  in
  !caught, result
;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none

let compare_pos pos loc =
  let pos = Misc.split_pos pos in
  if pos < Misc.split_pos loc.loc_start
  then -1
  else if pos > Misc.split_pos loc.loc_end
  then 1
  else 0

let union a b = match a,b with
  | a, { loc_ghost = true } -> a
  | { loc_ghost = true }, b -> b
  | a,b ->
    let loc_start =
      if Misc.split_pos a.loc_start <= Misc.split_pos b.loc_start
      then a.loc_start
      else b.loc_start
    and loc_end =
      if Misc.split_pos a.loc_end <= Misc.split_pos b.loc_end
      then b.loc_end
      else a.loc_end
    in
    { loc_start ; loc_end ; loc_ghost = a.loc_ghost && b.loc_ghost }
