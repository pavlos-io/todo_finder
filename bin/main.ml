open Core

(* Todo module *)
type todo = 
  { content : string
  ; line_nb : int
  ; pos : int
  }

let find_todo_in_line line_idx content = 
  let search = 
    content
    |> String.lowercase
    |> String.substr_index ~pattern:"todo:" in
  match search with
  | Some(pos) -> Some({content; pos; line_nb = line_idx + 1})
  | None      -> None
  
let find_todos_in_file contents = 
  contents
  |> String.split_lines
  |> List.mapi ~f:find_todo_in_line
  |> List.filter_opt

let print_todo {content; line_nb; pos} =
  printf "Line %d:%d: %s \n" line_nb pos content
(* /Todo module *)

(* Filesystem logic *)
let is_dir_explorable s = Char.(String.get s 0 <> '.')

let rec iter_dir ~f dirname =
  let open Core_unix in
  (* todo move Sys_unix.is_directory here *)
  let d = opendir dirname in 
  try while true do 
    match readdir_opt d with 
    | None    -> raise End_of_file
    | Some(s) -> let child = Filename.concat dirname s in
      match Sys_unix.is_directory child with
      | `Yes     -> if is_dir_explorable s then iter_dir ~f child
      | `No      -> f child
      | `Unknown -> failwith ("Unknown filetype: " ^ child)
  done with End_of_file -> closedir d

let read_file filename =
  let todos = filename
    |> Stdio.In_channel.read_all
    |> find_todos_in_file in
    if List.length todos > 0 then
      printf "\n%s:\n" filename;
      List.iter ~f:print_todo todos
(* /Filesystem logic *)

let get_input_filename_exn args =
  if Array.length args < 2 then
    failwith "Please provide a file/directory name, or `.` to scan the current directory!"
  else if Array.length args > 2 then
    failwith "Too many args!";
  Array.get args 1

let () = 
  Sys.get_argv()
  |> get_input_filename_exn
  |> iter_dir ~f:read_file