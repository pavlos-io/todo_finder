open Core
open Core_unix

type todo = 
  { content : string
  ; line_nb : int
  ; pos : int
  }

let find_todo line_idx content = 
  let search = 
    content
    |> String.lowercase
    |> String.substr_index ~pattern:"todo" in
  match search with
  | Some(pos) -> Some({content; pos; line_nb = line_idx + 1})
  | None      -> None

let print_todo {content; line_nb; pos} =
  printf "Line %d:%d: %s \n" line_nb pos content

let get_input_filename_exn args =
  if Array.length args < 2 then
    failwith "Please provide a file/directory name, or `.` to scan the current directory!"
  else if Array.length args > 2 then
    failwith "Too many args!";
  Array.get args 1

let find_todos_in_file contents = 
  contents
  |> String.split_lines
  |> List.mapi ~f:find_todo
  |> List.filter_opt

let () = 
  Sys.get_argv()
  |> get_input_filename_exn
  |> Stdio.In_channel.read_all
  |> find_todos_in_file
  |> List.iter ~f:print_todo

(* let iter_dir ~f dirname =
  let d = opendir dirname in
  try while true do 
    match readdir_opt d with 
    | Some(s) -> f s
    | None -> raise End_of_file
  done with End_of_file -> closedir d

  let () =
    (* Filename.split_extension "foo.bar" *)
    iter_dir ~f:(fun s-> printf "%s\n" s) "." *)