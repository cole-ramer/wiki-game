open! Core

module Article = struct
  module T = struct
    type t =
      { url : string
      ; title : string
      ; depth : int
      }
    [@@deriving sexp]

    let equal t1 t2 = String.equal t1.title t2.title
    let hash t = String.hash t.title
    let compare t1 t2 = String.compare t1.title t2.title
  end

  include T

  let create ~url ~title depth = { url; title; depth }

  module Hash_set = Hash_set.Make (T)
end

module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_attributes (v : Article.t) =
      [ `Shape `Box; `Label v.title; `Fillcolor 1000 ]
    ;;

    let vertex_name (v : Article.t) =
      String.filter v.title ~f:(fun c ->
        not (Char.equal c '-' || Char.equal c '(' || Char.equal c ')'))
      |> String.strip
      |> String.substr_replace_all ~pattern:" " ~with_:"_"
    ;;

    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
(*REVIST HERE NOT SURE IF THIS IS THE BEST APPROACH TO HANDLING ALL THE OPTIONS*)

let get_link a_node =
  let open Soup in
  match a_node |> element with
  | Some a_node_element ->
    (match attribute "href" a_node_element with
     | Some link -> link
     (* Subsitutes empty link for no link for increasing the usability of the get_link function,
        this works because is_link will fail b/c it does not have the /wiki/ substring*)
     | None -> "")
  | None -> ""
;;

let is_link link : bool = String.is_substring link ~substring:"/wiki/"

let is_not_namespace link : bool =
  match Wikipedia_namespace.namespace link with
  | None -> true
  | Some _ -> false
;;

let is_wiki_link_and_not_namespace a_node : bool =
  let link = get_link a_node in
  is_link link && is_not_namespace link
;;

let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a"
  |> to_list
  |> List.filter ~f:is_wiki_link_and_not_namespace
  |> List.map ~f:get_link
  (* Removes duplicate links *)
  |> List.sort ~compare:String.compare
  |> List.remove_consecutive_duplicates ~equal:String.equal
;;

let%expect_test "get_linked_articles_local" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Carnivore
    /wiki/Domestication_of_the_cat
    /wiki/Mammal
    /wiki/Species
    |}]
;;

let%expect_test "get_linked_articles_remote" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      Remote
      ~resource:"https://en.wikipedia.org/wiki/Endara"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Endara
    /wiki/Given_name
    /wiki/Gonzalo_Endara_Crow
    /wiki/Guido_J._Martinelli_Endara
    /wiki/Guillermo_Endara
    /wiki/Iv%C3%A1n_Endara
    /wiki/Main_Page
    /wiki/Surname
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* fetches an article (local or remote) based on the how_to_fetch and resource
   and returns the html and contents as a string*)
let get_contents ~resource ~how_to_fetch =
  File_fetcher.fetch_exn how_to_fetch ~resource
;;

(* outputs the title of an article based on a string of contents*)
let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

(* takes in a url and depth of an article, along with an object with the how_to_fetch details
   outputs an Article.t with the matching url, title, and depth*)
let create_article_t_from_url_and_depth url depth ~how_to_fetch : Article.t =
  let contents = get_contents ~resource:url ~how_to_fetch in
  let title = get_title contents in
  Article.create ~url ~title depth
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let visited_articles = Article.Hash_set.create () in
  let articles_to_visit = Queue.create () in
  let wiki_graph = G.create () in
  let origin_article =
    create_article_t_from_url_and_depth origin 0 ~how_to_fetch
  in
  Queue.enqueue articles_to_visit origin_article;
  let rec traverse () =
    match Queue.dequeue articles_to_visit with
    | None -> ()
    | Some (current_article : Article.t) ->
      if
        (not (Hash_set.mem visited_articles current_article))
        (* Note max_depth is the max_depth of verticies
           that we explore the children of*)
        && current_article.depth <= max_depth
      then (
        Hash_set.add visited_articles current_article;
        let current_article_contents =
          get_contents ~resource:current_article.url ~how_to_fetch
        in
        get_linked_articles current_article_contents
        |> List.iter ~f:(fun child_article_url ->
          let child_aritcle_t =
            create_article_t_from_url_and_depth
              child_article_url
              (current_article.depth + 1)
              ~how_to_fetch
          in
          (match
             ( G.mem_edge wiki_graph current_article child_aritcle_t
             , G.mem_edge wiki_graph child_aritcle_t current_article )
           with
           | false, false ->
             G.add_edge wiki_graph current_article child_aritcle_t
           | true, false | false, true | true, true -> ());
          (* reduces redundant dequeuing of already visited articles*)
          match Hash_set.mem visited_articles child_aritcle_t with
          | false -> Queue.enqueue articles_to_visit child_aritcle_t
          | true -> ()));
      (* Traverses as long as queue is not empty even if current_article has been visisted*)
      traverse ()
  in
  traverse ();
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    wiki_graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 3 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
