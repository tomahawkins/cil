open File
open Ast

module Self =
  Plugin.Register
    (struct
      let name = "DumpCIL"
      let shortname = "dumpcil"
      let descr = "Dumps CIL and ACSL to stdout."
    end);;

module Enabled =
  Self.False
    (struct
      let option_name = "-dumpcil"
      let descr = "Dumping CIL!"
    end);;

let dump _ = "some file\nline2\n"

let run () =
  File.init_from_cmdline ();
  print_endline (dump (Ast.get ()))

let () = Db.Main.extend (fun () -> if Enabled.get () then run ())


