open! Core
open Capturego_logic_library
open Hw2_capturego_logic
open Js_of_ocaml
open Virtual_dom
open Async_kernel
open! Bonsai.Let_syntax

let board_size = 19
let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let black_stone =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='45' stroke='black' stroke-width='3' fill='black' />"
    ()
;;

let white_stone =
  Vdom.Node.inner_html_svg
    ~tag:"svg"
    ~attrs:[ viewbox ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:
      "<circle cx='50' cy='50' r='45' stroke='black' stroke-width='3' fill='white' />"
    ()
;;

let lookup_cell (game_state : Game_state.t) ~row ~column = game_state.board.(row).(column)

type action = Place of int * int [@@deriving sexp]

module Action = struct
  type t = action [@@deriving sexp]
end


module Id = struct
  let generate () =
    let t = (new%js Js.date_now)##getTime |> Js.to_float |> int_of_float in
    let r = Random.int 0x3ffffff in
    Printf.sprintf "%x-%x" t r
  ;;
end

module Player_label = struct
  let make idx = Printf.sprintf "Player %d" (idx + 1)
end


module Firebase = struct
  module Config = struct
    let project = "ocaml-cttt"
    let key = "AIzaSyBsyzwDn-o2a47CAelN0kixWpFEryHuKGE"

    let base =
      Printf.sprintf
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
        project
    ;;

    let run_query =
      Printf.sprintf
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents:runQuery?key=%s"
        project
        key
    ;;

    (* single-document endpoints *)
    let lobby_doc id = Printf.sprintf "%s/lobby/%s?key=%s" base id key
    let game_doc id = Printf.sprintf "%s/game-state/%s?key=%s" base id key
    let lobby_create id = Printf.sprintf "%s/lobby?documentId=%s&key=%s" base id key
    let game_create id = Printf.sprintf "%s/game-state?documentId=%s&key=%s" base id key
  end

  module Http = struct
    type method_ =
      [ `GET
      | `POST
      | `PATCH
      ]

    let to_js = function
      | `GET -> Js.string "GET"
      | `POST -> Js.string "POST"
      | `PATCH -> Js.string "PATCH"
    ;;

    let request ~(meth : method_) ~(url : string) ?(body : string option) () =
      Deferred.create (fun ivar ->
        let xhr = XmlHttpRequest.create () in
        xhr##_open (to_js meth) (Js.string url) Js._true;
        (match body with
         | Some _ ->
           xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json")
         | None -> ());
        xhr##.onreadystatechange
        := Js.wrap_callback (fun _ ->
             match xhr##.readyState with
             | XmlHttpRequest.DONE ->
               let status = xhr##.status in
               let resp =
                 Js.Opt.get xhr##.responseText (fun () -> Js.string "") |> Js.to_string
               in
               Ivar.fill ivar (status, resp)
             | _ -> ());
        ignore
          (xhr##send
             (match body with
              | None -> Js.null
              | Some b -> Js.Opt.return (Js.string b))))
    ;;
  end

  module Json = struct
    let get o k = Js.Unsafe.get o k
    let opt o = Js.Optdef.test (Js.Optdef.return o)
    let to_string_exn v = Js.to_string v
    let int_of_jsint v = v |> Js.to_string |> Int.of_string
  end

  module Lobby = struct
    type t =
      { game_id : string
      ; capacity : int
      ; players : string list
      ; status : string
      }
    [@@deriving sexp, equal]

    let encode_create ~(game_id : string) ~(capacity : int) ~(first_player : string) =
      Printf.sprintf
        {|{"fields":{
            "game_id":{"stringValue":"%s"},
            "capacity":{"integerValue":"%d"},
            "players":{"arrayValue":{"values":[{"stringValue":"%s"}]}},
            "status":{"stringValue":"waiting"},
            "created_at":{"timestampValue":"%s"}
          }}|}
        game_id
        capacity
        first_player
        ((new%js Js.date_now)##toISOString |> Js.to_string)
    ;;

    let encode_players players =
      let vals =
        players
        |> List.map ~f:(fun p -> Printf.sprintf {|{"stringValue":"%s"}|} p)
        |> String.concat ~sep:","
      in
      Printf.sprintf {|{"fields":{"players":{"arrayValue":{"values":[%s]}}}}|} vals
    ;;

    let decode ~game_id json =
      let open Json in
      let fields = get json "fields" in
      let capacity =
        match opt fields with
        | false -> 0
        | true ->
          let v = get (get fields "capacity") "integerValue" in
          int_of_jsint v
      in
      let status =
        match opt fields with
        | false -> "waiting"
        | true ->
          let v = get (get fields "status") "stringValue" in
          to_string_exn v
      in
      let players =
        match opt fields with
        | false -> []
        | true ->
          let arr = get (get (get fields "players") "arrayValue") "values" in
          let n = arr##.length in
          let rec loop i acc =
            match Int.(i >= n) with
            | true -> List.rev acc
            | false ->
              let v = Js.Optdef.get (Js.array_get arr i) (Js.Unsafe.obj [||]) in
              let s = get v "stringValue" |> to_string_exn in
              loop (i + 1) (s :: acc)
          in
          loop 0 []
      in
      { game_id; capacity; players; status }
    ;;

    let run_query_first_waiting ~(capacity : int) =
      let where =
        Printf.sprintf
          {|{
              "structuredQuery":{
                "from":[{"collectionId":"lobby"}],
                "where":{"compositeFilter":{"op":"AND","filters":[
                  {"fieldFilter":{"field":{"fieldPath":"capacity"},"op":"EQUAL","value":{"integerValue":"%d"}}},
                  {"fieldFilter":{"field":{"fieldPath":"status"},"op":"EQUAL","value":{"stringValue":"waiting"}}}
                ]}},
                "limit":1
              }
            }|}
          capacity
      in
      let%bind.Deferred status, resp =
        Http.request ~meth:`POST ~url:Config.run_query ~body:where ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let arr = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        let first = Js.array_get arr 0 in
        let has_doc = Json.opt (Json.get first "document") in
        (match has_doc with
         | false -> Async_kernel.return None
         | true ->
           let doc = Json.get first "document" in
           let name = Json.get doc "name" |> Js.to_string in
           let game_id =
             match String.rsplit2 ~on:'/' name with
             | None -> name
             | Some (_p, id) -> id
           in
           let lobby = decode ~game_id doc in
           Async_kernel.return (Some lobby))
      | _ -> Async_kernel.return None
    ;;

    let get ~(game_id : string) =
      let%bind.Deferred status, resp =
        Http.request ~meth:`GET ~url:(Config.lobby_doc game_id) ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let json = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        Async_kernel.return (Ok (decode ~game_id json))
      | 404 -> Async_kernel.return (Error `Not_found)
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let create ~(game_id : string) ~(capacity : int) ~(first_player : string) =
      let body = encode_create ~game_id ~capacity ~first_player in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`POST ~url:(Config.lobby_create game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> get ~game_id
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let patch_players ~(game_id : string) ~(players : string list) =
      let body = encode_players players in
      let url = Config.lobby_doc game_id ^ "&updateMask.fieldPaths=players" in
      let%bind.Deferred status, _resp = Http.request ~meth:`PATCH ~url ~body () in
      match status with
      | s when s >= 200 && s < 300 -> get ~game_id
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let set_started ~(game_id : string) =
      let body = {|{"fields":{"status":{"stringValue":"started"}}}|} in
      let url = Config.lobby_doc game_id ^ "&updateMask.fieldPaths=status" in
      let%bind.Deferred status, _resp = Http.request ~meth:`PATCH ~url ~body () in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;
  end

  module Game_doc = struct
    type t =
      { game_id : string
      ; state : Game_state.t option
      }

    let encode_state (st : Game_state.t) =
      let s = Game_state.sexp_of_t st |> Sexp.to_string |> String.escaped in
      Printf.sprintf {|{"fields":{"state":{"stringValue":"%s"}}}|} s
    ;;

    let encode_create ~(game_id : string) ~(initial : Game_state.t) =
      let s = Game_state.sexp_of_t initial |> Sexp.to_string |> String.escaped in
      Printf.sprintf
        {|{"fields":{
            "game_id":{"stringValue":"%s"},
            "state":{"stringValue":"%s"},
            "created_at":{"timestampValue":"%s"}
          }}|}
        game_id
        s
        ((new%js Js.date_now)##toISOString |> Js.to_string)
    ;;

    let decode ~(game_id : string) json =
      let fields = Js.Unsafe.get json "fields" in
      let has_state = Json.opt (Js.Unsafe.get fields "state") in
      match has_state with
      | false -> { game_id; state = None }
      | true ->
        let s =
          Js.Unsafe.get (Js.Unsafe.get fields "state") "stringValue" |> Js.to_string
        in
        let sexp = Sexp.of_string s in
        let st = Game_state.t_of_sexp sexp in
        { game_id; state = Some st }
    ;;

    let create ~(game_id : string) ~(initial : Game_state.t) =
      let body = encode_create ~game_id ~initial in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`POST ~url:(Config.game_create game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | 409 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;

    let get ~(game_id : string) =
      let%bind.Deferred status, resp =
        Http.request ~meth:`GET ~url:(Config.game_doc game_id) ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let json = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        Async_kernel.return (Ok (decode ~game_id json))
      | 404 -> Async_kernel.return (Ok { game_id; state = None })
      | _ -> Async_kernel.return (Error ())
    ;;

    let save ~(game_id : string) ~(state : Game_state.t) =
      let body = encode_state state in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`PATCH ~url:(Config.game_doc game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;
  end
end

let capturego_board ~(game_state : Game_state.t) ~inject ~on_play_again ~last_error =
  let is_game_over = Decision.is_game_over game_state.decision in
  let game_over_text =
    match game_state.decision with
    | Decision.Winner Player_kind.Black -> Some "Black wins!"
    | Decision.Winner Player_kind.White -> Some "White wins!"
    | Decision.Stalemate -> Some "Stalemate!"
    | _ -> None
  in
  let capture_counters =
    let turn_text =
      match game_over_text with
      | Some txt -> txt
      | None -> (
        match game_state.decision with
        | Decision.In_progress { whose_turn } -> (
          match whose_turn with
          | Player_kind.Black -> "Black's Turn"
          | Player_kind.White -> "White's Turn")
        | _ -> "")
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "capture-counters" ]
      [ Vdom.Node.span
          ~attrs:[ Vdom.Attr.class_ "black-captures" ]
          [ Vdom.Node.text ("Black captures: " ^ Int.to_string game_state.black_captures)
          ]
      ; Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "turn-indicator" ] [ Vdom.Node.text turn_text ]
      ; Vdom.Node.span
          ~attrs:[ Vdom.Attr.class_ "white-captures" ]
          [ Vdom.Node.text ("White captures: " ^ Int.to_string game_state.white_captures)
          ]
      ]
  in
  let error_banner =
    match last_error with
    | Some (_row, _col, msg) when not (String.is_empty msg) ->
      Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "move-error-banner" ] [ Vdom.Node.text msg ]
    | _ -> Vdom.Node.none
  in
  let spacing_count = board_size - 1 in
  let pct_of i = Printf.sprintf "%.6f%%" (100. *. (Float.of_int i) /. Float.of_int spacing_count) in

  let intersection_nodes =
    List.concat_map (List.init board_size ~f:Fn.id) ~f:(fun row ->
      List.map (List.init board_size ~f:Fn.id) ~f:(fun column ->
        let cell_value = lookup_cell game_state ~row ~column in
        let top = pct_of row in
        let left = pct_of column in
        let stone_node =
          match cell_value with
          | Some stone ->
            let stone_vdom =
              match stone.owner with
              | Player_kind.Black -> black_stone
              | Player_kind.White -> white_stone
            in
            let extra_class =
              match game_state.last_move with
              | Some (Move.Place pos) when pos.row = row && pos.column = column ->
                " slowly_appear"
              | _ -> ""
            in
            Vdom.Node.div
              ~attrs:
                [ Vdom.Attr.class_
                    ("go-stone" ^ extra_class)
                ]
              [ stone_vdom ]
          | None -> Vdom.Node.none
        in
        let is_error_cell =
          match last_error with
          | Some (err_row, err_col, _msg) -> err_row = row && err_col = column
          | None -> false
        in
        let classes = if is_error_cell then "intersection cell-error" else "intersection" in
        Vdom.Node.div
          ~attrs:
            [ Vdom.Attr.class_ classes
            ; Vdom.Attr.create "style" (Printf.sprintf "top:%s; left:%s;" top left)
            ; Vdom.Attr.on_click (fun _ -> if is_game_over then Vdom.Effect.Ignore else inject (Place (row, column)))
            ]
    (if Option.is_none cell_value then [] else [ stone_node ])))
  in

  let board = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "go-board" ] intersection_nodes
  in
  let play_again_button =
    match game_over_text with
    | Some _ ->
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "game-over-message" ]
        [ Vdom.Node.button
            ~attrs:[ Vdom.Attr.on_click (fun _ -> on_play_again) ]
            [ Vdom.Node.text "Play Again" ]
        ]
    | None -> Vdom.Node.none
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "capturego-container" ]
    [ capture_counters; error_banner; board; play_again_button ]
;;

type setup_action = SetGoalCaptures of int [@@deriving sexp]

module Setup_action = struct
  type t = setup_action [@@deriving sexp]
end

let app =
  let%sub goal_captures, set_goal_captures = Bonsai.state (module Int) ~default_model:0 in
  let%sub game_started, set_game_started =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub game_state, set_game_state =
    Bonsai.state
      (module Game_state)
      ~default_model:(Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn)
  in
  let%sub last_error, set_last_error =
    Bonsai.state
      (module struct
        type t = (int * int * string) option [@@deriving sexp, equal]
      end)
      ~default_model:None
  in
  let%arr goal_captures = goal_captures
  and set_goal_captures = set_goal_captures
  and game_started = game_started
  and set_game_started = set_game_started
  and game_state = game_state
  and set_game_state = set_game_state
  and last_error = last_error
  and set_last_error = set_last_error in
  let inject action =
    match action with
    | Place (row, column) ->
      let move = Move.Place { Cell_position.row; column } in
      (match Game_state.make_move game_state move with
       | Ok new_model -> Ui_effect.Many [ set_game_state new_model; set_last_error None ]
       | Error err ->
         let msg =
           match err with
           | Game_state.Move_error.Game_is_over -> "Game is already over"
           | Game_state.Move_error.Space_already_filled -> "Space already filled"
           | Game_state.Move_error.Illegal_cell_position -> "Illegal cell position"
           | Game_state.Move_error.Ko_violation -> "Move violates Ko rule"
           | Game_state.Move_error.Self_capture_violation -> "Move would be self-capture"
         in
         Ui_effect.Many [ set_last_error (Some (row, column, msg)) ])
  in
  let on_play_again =
    Ui_effect.Many
      [ set_game_started false
      ; set_goal_captures 0
      ; set_game_state
          (Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn)
      ; set_last_error None
      ]
  in
  if not game_started
  then (
    let input_attrs =
      [ Vdom.Attr.type_ "number"
      ; Vdom.Attr.value (Int.to_string goal_captures)
      ; Vdom.Attr.min 1.
      ; Vdom.Attr.on_input (fun _ v ->
          match Int.of_string_opt v with
          | Some n when n > 0 -> set_goal_captures n
          | _ -> Vdom.Effect.Ignore)
      ]
    in
    let start_button =
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              if goal_captures > 0
              then
                Ui_effect.Many
                  [ set_game_state
                      (Game_state.create ~goal_captures |> Result.ok |> Option.value_exn)
                  ; set_game_started true
                  ; set_last_error None
                  ; Vdom.Effect.Ignore
                  ]
              else Vdom.Effect.Ignore)
          ]
        [ Vdom.Node.text "Start Game" ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "setup-screen" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "Capture Go" ]
      ; Vdom.Node.label [ Vdom.Node.text "Play to how many captures? " ]
      ; Vdom.Node.input ~attrs:input_attrs ()
      ; start_button
      ])
  else capturego_board ~game_state ~inject ~on_play_again ~last_error
;;

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"app" app
