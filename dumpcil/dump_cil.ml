open Ast
open Cil_types
open File
open Lexing
open List

let error loc msg = prerr_endline ("ERROR: " ^ msg); exit 1

let list a = if length a == 0 then "[]" else "[" ^ String.concat ", " a ^ "]"

let string a = "\"" ^ a ^ "\"" (*XXX Need to process meta chars. *)

let unknown_location =
  let a = { pos_fname = "unknown"
          ; pos_lnum = 0
          ; pos_bol  = 0
          ; pos_cnum = 0
          }
  in (a, a)

let format_fundec _ = "Fundec"

let format_location (a, _) = "Location" ^ " " ^ string a.pos_fname
                                        ^ " " ^ string_of_int a.pos_lnum
                                        ^ " " ^ string_of_int (a.pos_cnum - a.pos_bol + 1)

let unknown_global loc = "UnknownGlobal (" ^ format_location loc ^ ")"

let format_global a = match a with
    GType        (a, loc)    -> unknown_global loc
  | GCompTag     (a, loc)    -> unknown_global loc
  | GCompTagDecl (a, loc)    -> unknown_global loc
  | GEnumTag     (a, loc)    -> unknown_global loc
  | GEnumTagDecl (a, loc)    -> unknown_global loc
  | GVarDecl     (f, v, loc) -> unknown_global loc
  | GVar         (v, i, loc) -> unknown_global loc
  | GFun         (a, loc)    -> "GFun (" ^ format_fundec a ^ ") (" ^ format_location loc ^ ")"
  | GAsm         (_, loc)    -> unknown_global loc
  | GPragma      (a, loc)    -> unknown_global loc
  | GText        _           -> unknown_global unknown_location
  | GAnnot       (_, loc)    -> unknown_global loc


let format_file file =
  let globals = map format_global file.globals in
  "File"
  ^ " " ^ string file.fileName
  ^ " " ^ list (map format_global file.globals)
  ^ " " ^ (match file.globinit with
            None   -> "Nothing"
          | Some f -> "(Just (" ^ format_fundec f ^ "))")
  ^ " " ^ (if file.globinitcalled then "True" else "False")

let run () =
  File.init_from_cmdline ();
  print_endline (format_file (Ast.get ()))

module Self =
  Plugin.Register
    (struct
      let name = "dumpcil"
      let shortname = "dumpcil"
      let descr = "Dumps CIL and ACSL to stdout to be read by Haskell cil."
    end);;

module Enabled =
  Self.False
    (struct
      let option_name = "-dumpcil"
      let descr = "Dumps CIL and ACSL to stdout to be read by Haskell cil."
    end);;

let () = Db.Main.extend (fun () -> if Enabled.get () then run ())


