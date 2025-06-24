open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let is_known_for div =
  let open Soup in
  match element div with
  | Some div_element ->
    (match attribute "data-testid" div_element with
     | Some data_testid ->
       String.is_substring data_testid ~substring:"nm_kwn_for_"
     | None -> false)
  | None -> false
;;

let a_tag_has_correct_class a : bool =
  let open Soup in
  classes a
  |> List.exists ~f:(fun current_class ->
    String.equal "ipc-primary-image-list-card__title" current_class)
;;

let get_proper_a_tag_nodes_for_each_element known_for_div =
  let open Soup in
  tags "a" known_for_div
  |> filter a_tag_has_correct_class
  |> to_list
  |> List.hd_exn
;;

let get_credits contents : string list =
  let open Soup in
  let all_divs = parse contents $$ "div" in
  let known_for_divs = filter is_known_for all_divs in
  map get_proper_a_tag_nodes_for_each_element known_for_divs
  |> to_list
  |> List.map ~f:(fun a -> texts a |> String.concat |> String.strip)
;;

let%expect_test "get_credits" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      Remote
      ~resource:"https://www.imdb.com/name/nm0000375/"
  in
  List.iter (get_credits contents) ~f:print_endline;
  [%expect
    {|
    Iron Man
    Iron Man 3
    Sherlock Holmes
    Avengers: Endgame
    |}]
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
