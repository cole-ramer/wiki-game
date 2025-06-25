open! Core
module City = String

module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let get_all_city_pairs (cities : string list) : (string * string) list =
      List.concat_mapi
        (cities : string list)
        ~f:(fun index (city : string) ->
          List.concat_map
            (List.drop cities (index + 1))
            ~f:(fun city2 ->
              (* print_endline ("City" ^ city);
                 print_endline ("City2" ^ city2); *)
              [ city, city2; city2, city ]))
    ;;

    let of_string s =
      String.split s ~on:',' |> List.tl_exn |> get_all_city_pairs
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:Connection.of_string
    in
    (* print_endline
       (List.map connections ~f:(fun (a, b) -> "(" ^ a ^ "," ^ b ^ ") ")
       |> String.concat); *)
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values (whose types
           have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (person1, person2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          print_endline ("person1: " ^ person1 ^ " person2:" ^ person2);
          G.add_edge graph person1 person2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
