open! Core
open Capturego_logic_library
open Hw2_capturego_logic
open Virtual_dom
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

let lookup_cell (game_state : Game_state.t) ~row ~column =
  game_state.board.(row).(column)
;;

type action =
  | Place of int * int
[@@deriving sexp]

module Action = struct
  type t = action [@@deriving sexp]
end

let capturego_board ~(game_state : Game_state.t) ~inject =
  let is_game_over = Decision.is_game_over game_state.decision in
  let game_over_text =
    match game_state.decision with
    | Decision.Winner Player_kind.Black -> Some "Black wins!"
    | Decision.Winner Player_kind.White -> Some "White wins!"
    | Decision.Stalemate -> Some "Stalemate!"
    | _ -> None
  in
  let capture_counters =
    Vdom.Node.div
      ~attrs:[Vdom.Attr.class_ "capture-counters"]
      [ Vdom.Node.span ~attrs:[Vdom.Attr.class_ "black-captures"]
          [Vdom.Node.text ("Black captures: " ^ Int.to_string game_state.black_captures)]
      ; Vdom.Node.span ~attrs:[Vdom.Attr.class_ "white-captures"]
          [Vdom.Node.text ("White captures: " ^ Int.to_string game_state.white_captures)]
      ]
  in
  let board =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "go-board" ]
      (List.concat_map (List.init board_size ~f:(fun i -> i)) ~f:(fun row ->
        List.map (List.init board_size ~f:(fun i -> i)) ~f:(fun column ->
          let cell_value = lookup_cell game_state ~row ~column in
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
                | Some (Move.Place pos) when pos.row = row && pos.column = column -> " slowly_appear"
                | _ -> ""
              in
              Vdom.Node.div ~attrs:[Vdom.Attr.class_ ("go-stone" ^ extra_class)] [stone_vdom]
            | None -> Vdom.Node.none
          in
          Vdom.Node.div
            ~attrs:[
              Vdom.Attr.class_ "go-cell";
              Vdom.Attr.on_click (fun _ ->
                if is_game_over then Vdom.Effect.Ignore else inject (Place (row, column)))
            ]
            (if Option.is_none cell_value then [] else [stone_node])
        )
      ))
  in
  let game_over_message =
    match game_over_text with
    | Some txt -> Vdom.Node.div ~attrs:[Vdom.Attr.class_ "game-over-message"] [Vdom.Node.text txt]
    | None -> Vdom.Node.none
  in
  Vdom.Node.div
    ~attrs:[Vdom.Attr.class_ "capturego-container"]
    [ capture_counters; board; game_over_message ]
;;



type setup_action =
  | SetGoalCaptures of int
[@@deriving sexp]

module Setup_action = struct
  type t = setup_action [@@deriving sexp]
end

let app =
  let%sub goal_captures, set_goal_captures = Bonsai.state (module Int) ~default_model:0 in
  let%sub game_started, set_game_started = Bonsai.state (module Bool) ~default_model:false in
  let%sub game_state_and_inject =
    Bonsai.state_machine0
      (module Game_state)
      (module Action)
      ~default_model:(
        Game_state.create ~goal_captures:1
        |> Result.ok
        |> Option.value_exn
      )
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Place (row, column) ->
          let move = Move.Place { Cell_position.row = row; column } in
          match Game_state.make_move model move with
          | Ok new_model -> new_model
          | Error _ -> model
      )
  in
  let%arr goal_captures = goal_captures
  and set_goal_captures = set_goal_captures
  and game_started = game_started
  and set_game_started = set_game_started
  and game_state, inject = game_state_and_inject in
  if not game_started then
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
        ~attrs:[
          Vdom.Attr.on_click (fun _ ->
            if goal_captures > 0 then
              Ui_effect.Many [set_game_started true; Vdom.Effect.Ignore]
            else Vdom.Effect.Ignore
          )
        ]
        [Vdom.Node.text "Start Game"]
    in
    Vdom.Node.div
      ~attrs:[Vdom.Attr.class_ "setup-screen"]
      [ Vdom.Node.h2 [Vdom.Node.text "Capture Go"]
      ; Vdom.Node.label [Vdom.Node.text "Play to how many captures? "]
      ; Vdom.Node.input ~attrs:input_attrs ()
      ; start_button
      ]
  else
    capturego_board ~game_state ~inject
;;

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"app" app