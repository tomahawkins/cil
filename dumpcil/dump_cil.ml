open Ast
open Cil_types
open File
open Lexing
open List

let error loc msg = prerr_endline ("ERROR: " ^ msg); exit 1

let unknown =
  { pos_fname = "unknown"
  ; pos_lnum = 0
  ; pos_bol  = 0
  ; pos_cnum = 0
  }

let format_fundec _ _ = "function declaration"

let format_global a = match a with
    GType        (a, loc)    -> "typedef"
  | GCompTag     (a, loc)    -> "struct-union definition"
  | GCompTagDecl (a, loc)    -> "struct-union declaration"
  | GEnumTag     (a, loc)    -> "enum definition"
  | GEnumTagDecl (a, loc)    -> "enum declaration"
  | GVarDecl     (f, v, loc) -> "variable declaration"
  | GVar         (v, i, loc) -> "variable definition"
  | GFun         (a, loc)    -> format_fundec a loc
  | GAsm         (_, loc)    -> error loc "asm not supported"
  | GPragma      (a, loc)    -> ""
  | GText        _           -> "text"
  | GAnnot       _           -> "annot"

let dump file =
  (match file.globinit with
    None   -> ()
  | Some f -> error unknown "initializer not supported");
  iter (fun a -> if String.length a == 0 then () else print_endline a) (map format_global file.globals)

let run () =
  File.init_from_cmdline ();
  dump (Ast.get ())

module Self =
  Plugin.Register
    (struct
      let name = "DumpCIL"
      let shortname = "DumpCIL"
      let descr = "Dumps CIL and ACSL to stdout."
    end);;

module Enabled =
  Self.False
    (struct
      let option_name = "-dumpcil"
      let descr = "Dumps CIL and ACSL to stdout."
    end);;

let () = Db.Main.extend (fun () -> if Enabled.get () then run ())


