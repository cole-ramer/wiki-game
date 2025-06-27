open! Core

module Maze_point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  module Hash_set = Hash_set.Make (T)
end

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        (* let path = [] in
           let start = ref (0,0) in
           let final = ref (0,0) in
           let maze_as_a_2d_list = In_channel.read_lines (File_path.to_string input_file) |> List.map ~f:(String.split_on_chars ~on:['S'; 'E' ; '#'; '.'] ) in
           List.iteri maze_as_a_2d_list ~f:(fun row_index row -> List.iteri row ~f:(fun col_index symbol -> if (String.equal symbol "S") then start := (row_index, col_index); if (String.equal symbol "E") then final := (row_index, col_index) ));
           let visited_points =  Maze_point.Hash_set.create () in
           let rec dfs graph (row, col) visited : () =
           Hash_set.add visited (row, col);
           if row > 0 && not (Hash_set.mem visited ((row -1), col)) then
           match dfs graph ((row -1), col) visited with
           | None ->
           | Some ->
        *)
        ignore input_file;
        failwith "TODO"]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
