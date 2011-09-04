(*
  Picks N random lines from one or several files without scanning them
  entirely.
*)

(*
  TODO: make it work with files over 1GB on 32-bit systems.
  (would require to reimplement input_line to work directly
  on file descriptors, because Unix.LargeFile.lseek
  apparently causes input_line to misbehave)
*)

open Printf

let rec select f = function
    [] -> []
  | x :: l ->
      match f x with
          None -> select f l
        | Some y -> y :: select f l

let get_size fname =
  let x = Unix.stat fname in
  if x.Unix.st_kind <> Unix.S_REG then (
    eprintf "Ignoring %S: not a regular file\n%!" fname;
    None
  )
  else
    let size = x.Unix.st_size in
    if size > 0 then
      Some (fname, size)
    else
      None

let cumulate_sizes a =
  let b = Array.make (Array.length a) 0 in
  let sum = ref 0 in
  for i = 0 to Array.length a - 1 do
    let fname, size = a.(i) in
    sum := !sum + size;
    b.(i) <- !sum
  done;
  b, !sum

let get_next_line ic =
  ignore (input_line ic);
  try Some (input_line ic)
  with End_of_file -> None

let get_first_line ic =
  seek_in ic 0;
  input_line ic

let pick_line random_pos channels =
  let i, j = random_pos () in
  let ic = Lazy.force channels.(i) in
  seek_in ic j;
  let line =
    match get_next_line ic with
        None ->
          let i' = (i+1) mod (Array.length channels) in
          get_first_line (Lazy.force channels.(i'))
      | Some s ->
          s
  in
  printf "%s\n" line
  
let rec loop random_pos n channels =
  match n with
      Some 0 -> ()
    | _ ->
        pick_line random_pos channels;
        let n =
          match n with
              None -> None
            | Some i -> Some (i - 1)
        in
        loop random_pos n channels

let make_rng seed =
  let state =
    match seed with
        None -> Random.State.make_self_init ()
      | Some n -> Random.State.make [| n |]
  in
  fun n -> Int64.to_int (Random.State.int64 state (Int64.of_int n))

exception File_pos of (int * int)

let get_file_pos cumul abs_pos =
  let mini = ref 0 in
  try
    for i = 0 to Array.length cumul - 1 do
      let m = cumul.(i) in
      if abs_pos < m then (
        let rel_pos = abs_pos - !mini in
        raise (File_pos (i, rel_pos))
      )
      else
        mini := m
    done;
    assert false

  with File_pos x -> x

let run ~seed ~n files =
  let a = Array.of_list (select get_size files) in
  let b, total_bytes = cumulate_sizes a in
  if total_bytes = 0 then
    failwith "all empty files";
  let rng = make_rng seed in
  let random_pos () =
    let abs_pos = rng total_bytes in
    get_file_pos b abs_pos
  in
  let lazy_channels = Array.map (fun (fname, _) -> lazy (open_in fname)) a in
  loop random_pos n lazy_channels

let main () =
  let seed = ref None in
  let n = ref (Some 10) in
  let options = [
    "-i",
    Arg.Unit (fun () -> n := None),
    "
          Keep outputting random lines indefinitely instead of stopping
          after 10 lines. See also -n.";

    "-n",
    Arg.Int (fun i ->
               if i < 0 then
                 raise (Arg.Bad "Argument passed to -n may not be negative")
               else
                 n := Some i),
    "NUMBER
          Specify sample size, i.e. the number of lines to pick.
          The default is 10 lines. See also -i.";

    "-s",
    Arg.Int (fun x -> seed := Some x),
    "NUMBER
          Specify seed of the random number generator.
          By default, the seed is initialized randomly from system resources.";

    "-version",
    Arg.Unit (fun () -> print_endline Sampl_version.version; exit 0),
    "
          Print sampl version and exit.";
  ]
  in
  let usage_msg = sprintf "\
Usage: %s [OPTIONS] FILE1 [FILE2 ...]

sampl picks 10 random lines from the input files without scanning 
the entire file.

Options:
"
    Sys.argv.(0)
  in
  let files = ref [] in
  let anon_fun s =
    files := s :: !files in
  Arg.parse options anon_fun usage_msg;

  let files = List.rev !files in
  if files = [] then (
    Arg.usage options usage_msg;
    exit 1;
  );
  run ~seed: !seed ~n: !n files

let () =
  try
    Unix.handle_unix_error main ()
  with e ->
    eprintf "Error: %s\n%!" (Printexc.to_string e);
    exit 1
