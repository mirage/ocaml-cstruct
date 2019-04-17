let () = at_exit (fun () -> exit 0)
let () = Migrate_parsetree.Driver.run_main ()
