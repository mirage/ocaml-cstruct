
let mk_temp_dir () =
  let rand_num = Random.int 1000000 |> string_of_int in
  let tmp_dir = Filename.get_temp_dir_name () ^ "/test-cstruct-unix-" ^ rand_num in
  try
    Unix.mkdir tmp_dir 0o700;
    tmp_dir
  with Unix.Unix_error(Unix.EEXIST, _, _) ->
    (* re-use the old one *)
    tmp_dir
  | e -> raise (Sys_error ("Cannot create temp dir " ^ tmp_dir ^ " " ^ (Printexc.to_string e)))

let rmdir path =
  (* non-recursive for safety *)
  match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> Sys.remove (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path

let finally f g =
  try
    let r = f () in
    g ();
    r
  with e ->
    g ();
    raise e

let with_tmp_dir f =
  let dir = mk_temp_dir () in
  finally (fun () -> f dir) (fun () -> rmdir dir)

let test_message_list =[
  Cstruct.of_string "hello";
  Cstruct.of_string " ";
  Cstruct.of_string "cstruct";
  Cstruct.create 0;
  Cstruct.of_string " ";
  Cstruct.of_string "world";
]

let read_and_check fd sent_message =
  let expected = Cstruct.(to_string @@ concat sent_message) in
  let buf = Cstruct.create 1024 in
  let read = ref 0 in
  while !read < (String.length expected) do
    let n = Unix_cstruct.read fd (Cstruct.shift buf !read) in
    if n = 0 then raise End_of_file;
    read := !read + n
  done;
  let actual = Cstruct.(to_string @@ sub buf 0 !read) in
  Alcotest.(check string) "read contents" expected actual

let test_writev_file () =
  with_tmp_dir
    (fun dir ->
      let test = Filename.concat dir "test" in
      let fd = Unix.openfile test [ Unix.O_CREAT; Unix.O_RDWR ] 0o644 in
      finally (fun () ->
        Unix_cstruct.writev fd test_message_list;
        let ofs = Unix.lseek fd 0 Unix.SEEK_SET in
        Alcotest.(check int) "file offset" 0 ofs;
        read_and_check fd test_message_list
      ) (fun () -> Unix.close fd)
    )

let with_sock_stream f =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  finally (fun () -> f s) (fun () -> Unix.close s)

let localhost = Unix.inet_addr_of_string "127.0.0.1"

let bind_random_port s =
  Unix.bind s (Unix.ADDR_INET(localhost, 0));
  match Unix.getsockname s with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> assert false

let test_writev_socket () =
  with_sock_stream (fun s ->
    let port = bind_random_port s in
    Unix.listen s 1;
    let t = Thread.create (fun () ->
      let client, _ = Unix.accept s in
      finally
        (fun () ->
          read_and_check client test_message_list
        ) (fun () -> Unix.close client)
    ) () in
    with_sock_stream (fun c ->
      Unix.connect c (Unix.ADDR_INET(localhost, port));
      Unix_cstruct.writev c test_message_list;
    );
    Thread.join t
  )

let with_sock_dgram f =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  finally (fun () -> f s) (fun () -> Unix.close s)

let with_mutex m f =
  Mutex.lock m;
  finally f (fun () -> Mutex.unlock m)

let test_send_recv () =
  let test_message = Cstruct.concat test_message_list in
  with_sock_dgram (fun s ->
    let port = bind_random_port s in
    let m = Mutex.create () in
    let finished = ref false in
    let t = Thread.create (fun () ->
          let buf = Cstruct.create 1024 in
          let n = Unix_cstruct.recv s buf [] in
          Alcotest.(check int) "recv length" (Cstruct.length test_message) n;
          let expected = Cstruct.to_string test_message in
          let actual = Cstruct.(to_string @@ sub buf 0 n) in
          Alcotest.(check string) "read contents" expected actual;
          with_mutex m (fun () -> finished := true)
    ) () in
    with_sock_dgram (fun c ->
      Unix.connect c (Unix.ADDR_INET(localhost, port));
      while with_mutex m (fun () -> not !finished) do
        let n = Unix_cstruct.send c test_message [] in
        Alcotest.(check int) "send length" (Cstruct.length test_message) n;
        Thread.delay 0.1
      done
    );
    Thread.join t
  )

let test_sendto_recvfrom () =
  let test_message = Cstruct.concat test_message_list in
  with_sock_dgram @@ fun s ->
  with_sock_dgram @@ fun c ->
  let sport = bind_random_port s in
  let cport = bind_random_port c in (* So we can assert the sender on receiver port *)
  let server () =
    let buf = Cstruct.create 1024 in
    let n, (addr:Unix.sockaddr) = Unix_cstruct.recvfrom s buf [] in
    let addr, port = match addr with
      | ADDR_INET (a, p) -> Unix.string_of_inet_addr a, p
      | _ -> Alcotest.fail "Bad AF_FAMILY"
    in
    Alcotest.(check string) "recvfrom inetaddr" addr "127.0.0.1";
    Alcotest.(check int) "recvfrom port" port cport;
    Alcotest.(check int) "recvfrom length" (Cstruct.length test_message) n;
    let expected = Cstruct.to_string test_message in
    let actual = Cstruct.(to_string @@ sub buf 0 n) in
    Alcotest.(check string) "read contents" expected actual
  in
  let client () =
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, sport) in
    let n = Unix_cstruct.sendto c test_message [] addr in
    Alcotest.(check int) "sendto length" (Cstruct.length test_message) n;
  in
  client ();
  server ()

let suite = [
  "writev", [
    "test read and writev via a file", `Quick, test_writev_file;
    "test read and writev via a socket", `Quick, test_writev_socket;
  ];
  "send recv", [
    "test send and recv", `Quick, test_send_recv;
  ];
  "sendto recvfrom", [
    "test sendto and recvfrom", `Quick, test_sendto_recvfrom;
  ]
]

let () = Alcotest.run "cstruct.unix" suite
