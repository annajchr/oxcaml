open! Core
open Capturego_logic_library
open Hw2_capturego_logic

let ok_exn result =
  match result with
  | Ok v -> v
  | Error e -> failwith (Sexp.to_string [%sexp (e : _ list)])
;;

let ok_exn_move result =
  match result with
  | Ok v -> v
  | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)])
;;

let%expect_test "Game_state.create: valid and invalid goal_captures" =
  let print_result goal =
    let result = Game_state.create ~goal_captures:goal in
    print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
  in
  print_result 5;
  [%expect
    {|
    (Ok
     ((board
       ((() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())
        (() () () () () () () () () () () () () () () () () () ())))
      (previous_states ()) (goal_captures 5) (black_captures 0)
      (white_captures 0) (decision (In_progress (whose_turn White)))
      (last_move ())))
    |}];
  print_result 0;
  [%expect {| (Error (Goal_captures_less_than_one)) |}]
;;

let pretty_print_board (state : Game_state.t) =
  let board = state.board in
  for row = 0 to Array.length board - 1 do
    for col = 0 to Array.length board.(row) - 1 do
      match board.(row).(col) with
      | None -> print_string ". "
      | Some stone ->
        print_string
          (match stone.owner with
           | Player_kind.Black -> "B "
           | Player_kind.White -> "W ")
    done;
    print_endline ""
  done;
  print_s [%sexp (state.decision : Decision.t)]
;;

let random_walk_capturego ~random_seed =
  let initial_state = Game_state.create ~goal_captures:1 |> ok_exn in
  let rec walk state =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    if Decision.is_game_over random_state.decision
    then random_state
    else walk random_state
  in
  Core.Random.init random_seed;
  pretty_print_board (walk initial_state)
;;

let%expect_test "Capture Go random walk till terminal state (goal 1)" =
  random_walk_capturego ~random_seed:42;
  [%expect
    {|
    . W W . . W W W B . . W W B . B . . W
    . . . . W B . W . W B B B . . W W . B
    W . . . W W B W W . B B W . . B B W .
    B B W W . W W . . W B . W W . . W W B
    W . . . B B W . . . . . . W B . W . .
    . . . . B W W . . . W . . . W . W B W
    . W W B W W W . B B . . . W W . . . .
    . B . . W W B B W . . . B . W W . . W
    . . . W B B W W B B . . . . . . . . B
    B W . B . . . B B . B . . B W B B . .
    . B B . B . W B . . B . . W W . B B .
    W . . B B . . . W W W B B B B . B . .
    . W B B W W W B . B W B W . . B . B B
    W B . W W . W . . W W W B B . . B B B
    B W . . W . W W . . . W . W . B B B B
    . W . B B . B B . . . . . . B . W B .
    . B B . W B . . W B . W W . . . W . B
    . W . . . B B W . B . B B . B . W . .
    B . W . B . B . . B B W . W . B W . W
    (Winner Black)
    |}];
  random_walk_capturego ~random_seed:7;
  [%expect
    {|
    B B . W B B . . W . . W . B . B W . .
    W . . . W . . . B W W . W . W B W W W
    . . B B W B . W . B B B . . . W B W .
    . B B . . . . . W . B B . . . B . . .
    B . . W W . W . W . . B . . W W . . W
    W W W W W . W . . W B . W B W . . B B
    . W . W W B W B . . B B W . . . . W B
    . B B W . B . W W . . . W B . W . . B
    . . W B B W B . W . B B . W W B B . .
    W W B . . W W . W . B B W B . . . . W
    . . B . W W . B . B B B W B . . B W .
    W . W W . W B B W . W W . B W W B B B
    . . . B W B . W . . . B W . W . B . .
    B B . B W B . . B . . . B . B . . W .
    . B W . . . W . B . . B . . W W B W B
    W . W . B . . B . B . B . . B . . B B
    B . . . W B . . B B B B . B W B B . .
    B B . W . W . . . W . W . . . . . B W
    W W W W . . W . B . W . B . W . . . B
    (Winner Black)
    |}]
;;

let pretty_print_board (state : Game_state.t) =
  let board = state.board in
  for row = 0 to Array.length board - 1 do
    for col = 0 to Array.length board.(row) - 1 do
      match board.(row).(col) with
      | None -> print_string ". "
      | Some stone ->
        print_string
          (match stone.owner with
           | Player_kind.Black -> "B "
           | Player_kind.White -> "W ")
    done;
    print_endline ""
  done;
  print_s [%sexp (state.decision : Decision.t)]
;;

let%expect_test "Game_state.make_move: place stones and capture" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let moves =
    [ Move.Place { row = 0; column = 0 }
    ; (* White *)
      Move.Place { row = 1; column = 0 }
    ; (* Black *)
      Move.Place { row = 0; column = 1 }
    ; (* White *)
      Move.Place { row = 1; column = 1 }
    ; (* Black *)
      Move.Place { row = 0; column = 2 }
    ; (* White, surrounds black at (1,1) *)
      Move.Place { row = 1; column = 2 }
    ; (* Black *)
      Move.Place { row = 2; column = 1 } (* White, should capture black at (1,1) *)
    ]
  in
  let rec play_moves state moves =
    match moves with
    | [] -> state
    | m :: ms ->
      (match Game_state.make_move state m with
       | Ok s -> play_moves s ms
       | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)]))
  in
  let final_state = play_moves state moves in
  pretty_print_board final_state;
  [%expect
    {xxx|
    W W W . . . . . . . . . . . . . . . .
    B B B . . . . . . . . . . . . . . . .
    . W . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn Black))
    |xxx}]
;;

let%expect_test "Game_state.make_move: illegal moves" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let result1 = Game_state.make_move state (Move.Place { row = 19; column = 0 }) in
  print_s [%sexp (result1 : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Illegal_cell_position) |}];
  let state2 =
    Game_state.make_move state (Move.Place { row = 0; column = 0 }) |> ok_exn_move
  in
  let result2 = Game_state.make_move state2 (Move.Place { row = 0; column = 0 }) in
  print_s [%sexp (result2 : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Space_already_filled) |}]
;;

let%expect_test "Game_state.make_move: pass move" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let state2 = Game_state.make_move state Move.Pass |> ok_exn_move in
  print_s [%sexp (state2.decision : Decision.t)];
  [%expect {| (In_progress (whose_turn Black)) |}]
;;

let%expect_test "Game_state.make_move: win by capture" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let moves =
    [ Move.Place { row = 0; column = 0 }
    ; (* White *)
      Move.Place { row = 1; column = 0 }
    ; (* Black *)
      Move.Place { row = 0; column = 1 }
    ; (* White *)
      Move.Place { row = 1; column = 1 }
    ; (* Black *)
      Move.Place { row = 0; column = 2 }
    ; (* White *)
      Move.Place { row = 1; column = 2 }
    ; (* Black *)
      Move.Place { row = 2; column = 1 }
    ; (* White, should capture black at (1,1) *)
      Move.Place { row = 2; column = 0 }
    ; (* Black *)
      Move.Place { row = 2; column = 2 }
      (* White, should capture black at (1,2) and win *)
    ]
  in
  let rec play_moves state moves =
    match moves with
    | [] -> state
    | m :: ms ->
      (match Game_state.make_move state m with
       | Ok s -> play_moves s ms
       | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)]))
  in
  let final_state = play_moves state moves in
  pretty_print_board final_state;
  [%expect
    {|
    W W W . . . . . . . . . . . . . . . .
    B B B . . . . . . . . . . . . . . . .
    B W W . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn Black))
    |}]
;;
