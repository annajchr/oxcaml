open! Core
open Capturego_logic_library
open Hw2_capturego_logic
open Virtual_dom
open Js_of_ocaml
open Async_kernel
open! Bonsai.Let_syntax

module Multiplayer = struct
  let firebase_api_key = "AIzaSyBW10tGiRlPO4bhlmkoQPhz1akEeinPYN8"
  let project_id = "capturegofirebase"

  type player_slots =
    { white : string option
    ; black : string option
    }

  type document_fetch =
    | Not_found
    | Found of
        { state : Game_state.t option
        ; players : player_slots
        }

  let base_url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
      project_id
  ;;

  let game_document_url game_id =
    Printf.sprintf "%s/games/%s?key=%s" base_url game_id firebase_api_key
  ;;

  let state_update_url game_id =
    Printf.sprintf
      "%s/games/%s?updateMask.fieldPaths=state&key=%s"
      base_url
      game_id
      firebase_api_key
  ;;

  let players_update_url game_id =
    Printf.sprintf
      "%s/games/%s?updateMask.fieldPaths=players&key=%s"
      base_url
      game_id
      firebase_api_key
  ;;

  let generate_game_id () =
    Random.self_init ();
    let timestamp = int_of_float (Js.to_float (new%js Js.date_now)##getTime) in
    let random_suffix = Random.int 10_000 in
    Printf.sprintf "%d-%04d" timestamp random_suffix
  ;;

  let generate_client_id () =
    Random.self_init ();
    let alphabet = "abcdefghijklmnopqrstuvwxyz0123456789" in
    String.init 16 ~f:(fun _ -> alphabet.[Random.int (String.length alphabet)])
  ;;

  let normalize_game_id id =
    let stripped = String.strip id in
    String.map stripped ~f:(function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '-'
      | '_' as c -> c
      | _ -> '-')
  ;;

  let parse_players fields =
    let extract name =
      try
        let players_field = Js.Unsafe.get fields "players" in
        if not (Js.Optdef.test (Js.Optdef.return players_field))
        then None
        else
          let map_value = Js.Unsafe.get players_field "mapValue" in
          if not (Js.Optdef.test (Js.Optdef.return map_value))
          then None
          else
            let map_fields = Js.Unsafe.get map_value "fields" in
            let slot_field = Js.Unsafe.get map_fields name in
            if not (Js.Optdef.test (Js.Optdef.return slot_field))
            then None
            else
              let string_value = Js.Unsafe.get slot_field "stringValue" in
              if not (Js.Optdef.test (Js.Optdef.return string_value))
              then None
              else (
                let value = Js.to_string string_value in
                if String.is_empty value then None else Some value)
      with
      | _ -> None
    in
    { white = extract "white"; black = extract "black" }
  ;;

  let parse_document response_text =
    try
      let json = Js.Unsafe.global##._JSON##parse response_text in
      let fields = Js.Unsafe.get json "fields" in
      if not (Js.Optdef.test (Js.Optdef.return fields))
      then Ok (Found { state = None; players = { white = None; black = None } })
      else (
        let state_field = Js.Unsafe.get fields "state" in
        let state =
          if Js.Optdef.test (Js.Optdef.return state_field)
          then (
            let state_value = Js.Unsafe.get state_field "stringValue" in
            if Js.Optdef.test (Js.Optdef.return state_value)
            then (
              try
                Ok
                  (Some
                     (state_value
                      |> Js.to_string
                      |> Sexp.of_string
                      |> Game_state.t_of_sexp))
              with
              | exn ->
                Error (Printf.sprintf "Parse error: %s" (Exn.to_string exn)))
            else Ok None)
          else Ok None
        in
        match state with
        | Error _ as err -> err
        | Ok state ->
          let players = parse_players fields in
          Ok (Found { state; players }))
    with
    | exn -> Error (Printf.sprintf "Parse error: %s" (Exn.to_string exn))
  ;;

  let fetch_document_async ~game_id : (document_fetch, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "GET") (Js.string (game_document_url game_id)) Js._true;
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status = 404
           then Ivar.fill ivar (Ok Not_found)
           else if status >= 200 && status < 300
           then (
             try
               let response_text =
                 Js.Opt.get xhr##.responseText (fun () -> Js.string "{}")
               in
               (match parse_document response_text with
                | Ok parsed -> Ivar.fill ivar (Ok parsed)
                | Error msg -> Ivar.fill ivar (Error msg))
             with
             | exn ->
               Ivar.fill
                 ivar
                 (Error (Printf.sprintf "Parse error: %s" (Exn.to_string exn))))
           else
             Ivar.fill
               ivar
               (Error (Printf.sprintf "Failed to fetch: %d" status))
         | _ -> ());
    ignore (xhr##send Js.null);
    Ivar.read ivar
  ;;

  let fetch_document_effect ~game_id =
    Bonsai_web.Effect.of_deferred_fun (fun () -> fetch_document_async ~game_id) ()
  ;;

  let players_to_body { white; black } =
    let safe_string = function
      | None -> ""
      | Some s -> String.escaped s
    in
    Printf.sprintf
      {|{"fields":{"players":{"mapValue":{"fields":{
        "white":{"stringValue":"%s"},
        "black":{"stringValue":"%s"}
      }}}}}|}
      (safe_string white)
      (safe_string black)
  ;;

  let save_game_state_async ~game_id ~game_state
    : (unit, string) Result.t Deferred.t
    =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PATCH") (Js.string (state_update_url game_id)) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let game_state_sexp = Game_state.sexp_of_t game_state |> Sexp.to_string |> String.escaped in
    let body = Printf.sprintf {|{"fields":{"state":{"stringValue":"%s"}}}|} game_state_sexp in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else
             Ivar.fill
               ivar
               (Error (Printf.sprintf "Failed to save: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let save_game_state_effect ~game_id ~game_state =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> save_game_state_async ~game_id ~game_state)
      ()
  ;;

  let create_game_async ~game_id ~game_state ~player_id
    : (unit, string) Result.t Deferred.t
    =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    let url =
      Printf.sprintf "%s/games?documentId=%s&key=%s" base_url game_id firebase_api_key
    in
    xhr##_open (Js.string "POST") (Js.string url) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let game_state_sexp = Game_state.sexp_of_t game_state |> Sexp.to_string in
    let body =
      Printf.sprintf
        {|{"fields":{
        "state":{"stringValue":"%s"},
        "players":{"mapValue":{"fields":{
          "white":{"stringValue":"%s"},
          "black":{"stringValue":""}
        }}}
      }}|}
        (String.escaped game_state_sexp)
        (String.escaped player_id)
    in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           let response_text = Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else
             Ivar.fill
               ivar
                (Error
                   (Printf.sprintf
                      "Failed to create game: %d. Response: %s"
                      status
                      response_text))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let create_game_effect ~game_id ~game_state ~player_id =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> create_game_async ~game_id ~game_state ~player_id)
      ()
  ;;

  let claim_seat_async ~game_id ~seat ~client_id ~players
    : (player_slots, string) Result.t Deferred.t
    =
    let updated_players : player_slots =
      match seat with
      | `White -> { players with white = Some client_id }
      | `Black -> { players with black = Some client_id }
    in
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PATCH") (Js.string (players_update_url game_id)) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let body = players_to_body updated_players in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok updated_players)
           else
             Ivar.fill
               ivar
               (Error (Printf.sprintf "Failed to join game: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let claim_seat_effect ~game_id ~seat ~client_id ~players =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> claim_seat_async ~game_id ~seat ~client_id ~players)
      ()
  ;;
end

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

let client_id = Multiplayer.generate_client_id ()

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
  let%sub current_game_id, set_current_game_id =
    Bonsai.state
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
      ~default_model:None
  in
  let%sub game_id_input, set_game_id_input =
    Bonsai.state
      (module struct
        type t = string [@@deriving sexp, equal]
      end)
      ~default_model:""
  in
  let%sub setup_error, set_setup_error =
    Bonsai.state
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
      ~default_model:None
  in
  let%sub () =
    match%sub current_game_id with
    | None -> Bonsai.const ()
    | Some game_id ->
      let%sub poll_effect =
        let%arr game_state = game_state
        and set_game_state = set_game_state
        and set_last_error = set_last_error
        and set_game_started = set_game_started
        and game_started = game_started
        and game_id = game_id in
        let open Vdom.Effect.Let_syntax in
        let%bind result = Multiplayer.fetch_document_effect ~game_id in
        match result with
        | Ok Multiplayer.Not_found ->
          set_last_error (Some (-1, -1, "Game not found"))
        | Ok (Multiplayer.Found { state; players }) ->
          let both_ready = Option.is_some players.white && Option.is_some players.black in
          let%bind () =
            match state with
            | Some remote_state when not (Game_state.equal remote_state game_state) ->
              set_game_state remote_state
            | _ -> Vdom.Effect.Ignore
          in
          if both_ready && not game_started
          then set_game_started true
          else if (not both_ready) && game_started
          then set_game_started false
          else Vdom.Effect.Ignore
        | Error msg -> set_last_error (Some (-1, -1, "Sync failed: " ^ msg))
      in
      Bonsai.Clock.every
        ~when_to_start_next_effect:`Every_multiple_of_period_blocking
        (Time_ns.Span.of_sec 1.5)
        poll_effect
  in
  let%arr goal_captures = goal_captures
  and set_goal_captures = set_goal_captures
  and game_started = game_started
  and set_game_started = set_game_started
  and game_state = game_state
  and set_game_state = set_game_state
  and last_error = last_error
  and set_last_error = set_last_error
  and current_game_id = current_game_id
  and set_current_game_id = set_current_game_id
  and game_id_input = game_id_input
  and set_game_id_input = set_game_id_input
  and setup_error = setup_error
  and set_setup_error = set_setup_error in
  let inject action =
    match action with
    | Place (row, column) ->
      let move = Move.Place { Cell_position.row; column } in
      match Game_state.make_move game_state move with
      | Ok new_model ->
        let open Vdom.Effect.Let_syntax in
        let%bind () = set_game_state new_model in
        let%bind () = set_last_error None in
        (match current_game_id with
         | Some game_id ->
           let%bind result =
             Multiplayer.save_game_state_effect ~game_id ~game_state:new_model
           in
           (match result with
            | Ok () -> Vdom.Effect.Ignore
            | Error msg -> set_last_error (Some (row, column, "Sync failed: " ^ msg)))
         | None -> Vdom.Effect.Ignore)
      | Error err ->
        let msg =
          match err with
          | Game_state.Move_error.Game_is_over -> "Game is already over"
          | Game_state.Move_error.Space_already_filled -> "Space already filled"
          | Game_state.Move_error.Illegal_cell_position -> "Illegal cell position"
          | Game_state.Move_error.Ko_violation -> "Move violates Ko rule"
          | Game_state.Move_error.Self_capture_violation -> "Move would be self-capture"
        in
        set_last_error (Some (row, column, msg))
  in
  let on_play_again =
    Vdom.Effect.Many
      [ set_game_started false
      ; set_goal_captures 0
      ; set_game_state
          (Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn)
      ; set_last_error None
      ; set_current_game_id None
      ; set_setup_error None
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
    let start_local_button =
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.on_click (fun _ ->
              if goal_captures < 1
              then set_setup_error (Some "Goal captures must be at least 1")
              else (
                match Game_state.create ~goal_captures with
                | Error _ -> set_setup_error (Some "Goal captures must be at least 1")
                | Ok new_state ->
                  let open Vdom.Effect.Let_syntax in
                  let%bind () = set_game_state new_state in
                  let%bind () = set_game_started true in
                  let%bind () = set_last_error None in
                  let%bind () = set_setup_error None in
                  let%bind () = set_current_game_id None in
                  Vdom.Effect.Ignore))
          ]
        [ Vdom.Node.text "Start Local Game" ]
    in
    let handle_generate_id_click () =
      Vdom.Effect.Many
        [ set_game_id_input (Multiplayer.generate_game_id ()); set_setup_error None ]
    in
    let generate_id_button =
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> handle_generate_id_click ()) ]
        [ Vdom.Node.text "Generate Game ID" ]
    in
    let handle_multiplayer_click () =
      let normalized_id = Multiplayer.normalize_game_id game_id_input in
      if String.is_empty normalized_id
      then set_setup_error (Some "Enter a game ID to play online")
      else begin
        let open Vdom.Effect.Let_syntax in
        let%bind () = set_setup_error None in
        let%bind () = set_game_id_input normalized_id in
        let%bind fetch_result = Multiplayer.fetch_document_effect ~game_id:normalized_id in
        match fetch_result with
        | Error msg -> set_setup_error (Some ("Failed to load game: " ^ msg))
        | Ok Multiplayer.Not_found ->
          (match Game_state.create ~goal_captures with
           | Error _ -> set_setup_error (Some "Goal captures must be at least 1")
           | Ok new_state ->
             let%bind () = set_game_state new_state in
             let%bind () = set_goal_captures new_state.goal_captures in
             let%bind () = set_current_game_id (Some normalized_id) in
             let%bind () = set_game_started false in
             let%bind () = set_last_error None in
             let%bind create_result =
               Multiplayer.create_game_effect
                 ~game_id:normalized_id
                 ~game_state:new_state
                 ~player_id:client_id
             in
             (match create_result with
              | Ok () -> Vdom.Effect.Ignore
              | Error msg -> set_setup_error (Some msg)))
        | Ok (Multiplayer.Found { state; players }) ->
          let { Multiplayer.white = white_player; black = black_player } = players in
          let seat_for_client =
            if Option.value_map white_player ~default:false ~f:(String.equal client_id)
            then Some (`White, players)
            else if Option.value_map black_player ~default:false ~f:(String.equal client_id)
            then Some (`Black, players)
            else None
          in
          let available_seat =
            match seat_for_client with
            | Some (seat, current) -> Ok (seat, current)
            | None ->
              if Option.is_none white_player
              then Ok (`White, players)
              else if Option.is_none black_player
              then Ok (`Black, players)
              else Error "Game already has two players"
          in
          (match available_seat with
           | Error msg -> set_setup_error (Some msg)
           | Ok (seat, existing_players) ->
             let apply_join updated_players =
               let { Multiplayer.white; black } = updated_players in
               let both_ready = Option.is_some white && Option.is_some black in
               let open Vdom.Effect.Let_syntax in
               let%bind () = set_current_game_id (Some normalized_id) in
               let%bind () = set_last_error None in
               let%bind () =
                 match state with
                 | Some remote_state ->
                   let%bind () = set_game_state remote_state in
                   set_goal_captures remote_state.goal_captures
                 | None ->
                   (match Game_state.create ~goal_captures with
                    | Error _ -> set_setup_error (Some "Goal captures must be at least 1")
                    | Ok new_state -> set_game_state new_state)
               in
               set_game_started both_ready
             in
             (match seat_for_client with
              | Some _ -> apply_join existing_players
              | None ->
                let%bind players_result =
                  Multiplayer.claim_seat_effect
                    ~game_id:normalized_id
                    ~seat
                    ~client_id
                    ~players:existing_players
                in
                (match players_result with
                 | Error msg -> set_setup_error (Some msg)
                 | Ok updated_players -> apply_join updated_players)))
      end
    in
    let multiplayer_button =
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> handle_multiplayer_click ()) ]
        [ Vdom.Node.text "Host/Join Multiplayer" ]
    in
    let setup_error_node =
      match setup_error with
      | None -> Vdom.Node.none
      | Some msg ->
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "move-error-banner" ]
          [ Vdom.Node.text msg ]
    in
    let waiting_node =
      match current_game_id, game_started with
      | Some gid, false ->
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "capture-counters" ]
          [ Vdom.Node.span
              [ Vdom.Node.text ("Waiting for opponent... Game ID: " ^ gid) ]
          ]
      | _ -> Vdom.Node.none
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "setup-screen" ]
      [ Vdom.Node.h2 [ Vdom.Node.text "Capture Go" ]
      ; Vdom.Node.label [ Vdom.Node.text "Play to how many captures? " ]
      ; Vdom.Node.input ~attrs:input_attrs ()
      ; start_local_button
      ; Vdom.Node.div
          [ Vdom.Node.label [ Vdom.Node.text "Game ID (share with opponent): " ]
          ; Vdom.Node.input
              ~attrs:
                [ Vdom.Attr.type_ "text"
                ; Vdom.Attr.value game_id_input
                ; Vdom.Attr.placeholder "e.g., 1700000000-1234"
                ; Vdom.Attr.on_input (fun _ v ->
                    let trimmed = Multiplayer.normalize_game_id v in
                    set_game_id_input trimmed)
                ]
              ()
          ; generate_id_button
          ]
      ; multiplayer_button
      ; waiting_node
      ; setup_error_node
      ])
  else capturego_board ~game_state ~inject ~on_play_again ~last_error
;;

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"app" app
