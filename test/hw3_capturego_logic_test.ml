open! Core
open Capturego_logic_library
open Hw2_capturego_logic

let rec play_moves state moves =
  match moves with
  | [] -> state
  | m :: ms ->
    match Game_state.make_move state m with
    | Ok s -> play_moves s ms
    | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)])

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
  print_s [%sexp (state.decision : Decision.t)];
  Stdio.printf "Black captures: %d\n" state.black_captures;
  Stdio.printf "White captures: %d\n" state.white_captures;
  Stdio.printf "Goal captures: %d\n" state.goal_captures;
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
    . B B . W B . . W B . W W . . . . . B
    . W . . . B B W . B . B B . B . W . .
    B . W . B . B . . B B W . W . B W . W
    (Winner Black)
    Black captures: 1
    White captures: 0
    Goal captures: 1
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
    W . W W . . W . B . W . B . W . . . B
    (Winner Black)
    Black captures: 1
    White captures: 0
    Goal captures: 1
    |}]
;;

let%expect_test "Game_state.make_move: successful run to game win" =
  let state = Game_state.create ~goal_captures:3 |> ok_exn in
  (* Full run of first to three capture go game including alternating captures. *)
  let moves = [
    (* Black captures White's stone at 1,1 *)
  Move.Place { row = 1; column = 1 }; (* White *)
  Move.Place { row = 0; column = 1 }; (* Black *)
  Move.Place { row = 15; column = 15 }; (* White *)
  Move.Place { row = 1; column = 0 }; (* Black *)
  Move.Place { row = 15; column = 16 }; (* White *)
  Move.Place { row = 2; column = 1 }; (* Black *)
  Move.Place { row = 5; column = 5 }; (* White *)
  Move.Place { row = 1; column = 2 }; (* Black *)

  Move.Place { row = 10; column = 10 }; (* White *)
  Move.Place { row = 0; column = 18 }; (* Black *)
  Move.Place { row = 1; column = 18 }; (* White *)
  Move.Place { row = 18; column = 0 }; (* Black *)
  Move.Place { row = 0; column = 17 }; (* White *)
  Move.Place { row = 18; column = 18 }; (* Black *)

  Move.Place { row = 10; column = 11 }; (* White *)
  Move.Place { row = 14; column = 15 }; (* Black *)
  Move.Place { row = 10; column = 12 }; (* White *)
  Move.Place { row = 16; column = 15 }; (* Black *)
  Move.Place { row = 10; column = 13 }; (* White *)
  Move.Place { row = 15; column = 14 }; (* Black *)
  Move.Place { row = 10; column = 14 }; (* White *)
  Move.Place { row = 15; column = 17 }; (* Black *)
  Move.Place { row = 10; column = 15 }; (* White *)
  Move.Place { row = 16; column = 16 }; (* Black *)
  Move.Place { row = 10; column = 16 }; (* White *)
  Move.Place { row = 14; column = 16 }; (* Black *)
  ] in
  let final_state = play_moves state moves in
  pretty_print_board final_state;
  [%expect {|
    . B . . . . . . . . . . . . . . . W .
    B . B . . . . . . . . . . . . . . . W
    . B . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . W . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . W W W W W W W . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . B B . .
    . . . . . . . . . . . . . . B . . B .
    . . . . . . . . . . . . . . . B B . .
    . . . . . . . . . . . . . . . . . . .
    B . . . . . . . . . . . . . . . . . B
    (Winner Black)
    Black captures: 3
    White captures: 1
    Goal captures: 3 |}];
;;

let%expect_test "Game_state.make_move: place stones and capture 1 stone" =
  let state = Game_state.create ~goal_captures:3 |> ok_exn in
  let moves =
    [ Move.Place { row = 1; column = 1 } (* White *)
    ; Move.Place { row = 0; column = 1 } (* Black: top *)
    ; Move.Pass (* White: passes every move to allow capture *)
    ; Move.Place { row = 1; column = 0 } (* Black: left *)
    ; Move.Pass 
    ; Move.Place { row = 2; column = 1 } (* Black: bottom *)
    ; Move.Pass
    ; Move.Place { row = 1; column = 2 } (* Black: right, captures white stone at (1,1) *)
    ]
  in
  let final_state = play_moves state moves in
  pretty_print_board final_state;
  [%expect {|
    . B . . . . . . . . . . . . . . . . .
    B . B . . . . . . . . . . . . . . . .
    . B . . . . . . . . . . . . . . . . .
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
    (In_progress (whose_turn White))
    Black captures: 1
    White captures: 0
    Goal captures: 3 |}];
;;

let%expect_test "Game_state.make_move: illegal cell position error" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let result = Game_state.make_move state (Move.Place { row = 19; column = 0 }) in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Illegal_cell_position) |}]

let%expect_test "Game_state.make_move: space already filled error" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in

  (* Attempts to place stone in 0,0 after it is occupied. *)
  let moves = [ Move.Place { row = 0; column = 0 }; 
                Move.Place { row = 0; column = 0 } ] in
  let result =
    match Game_state.make_move state (List.hd_exn moves) with
    | Error e -> Error e
    | Ok s -> Game_state.make_move s (List.nth_exn moves 1)
  in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Space_already_filled) |}]

let%expect_test "Game_state.make_move: Game_is_over error" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in

  (* Same moves as 1 stone capture test case *)
  let moves =
    [ Move.Place { row = 1; column = 1 }
    ; Move.Place { row = 0; column = 1 }
    ; Move.Pass
    ; Move.Place { row = 1; column = 0 }
    ; Move.Pass 
    ; Move.Place { row = 2; column = 1 }
    ; Move.Pass
    ; Move.Place { row = 1; column = 2 } (* Black: captures white stone at (1,1) *)
    ]
  in
  let final_state = play_moves state moves in

  (* Try to make a move after the game is over *)
  let result_after = Game_state.make_move final_state (Move.Place { row = 2; column = 2 }) in
  print_s [%sexp (result_after : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Game_is_over) |}]

let%expect_test "Game_state.make_move: Self_capture_violation error" =
  let state = Game_state.create ~goal_captures:5 |> ok_exn in
  (* White creates a perimeter around a 2x2 square. *)
  let perimeter_moves = [
    Move.Place { row = 9; column = 10 }; Move.Pass;
    Move.Place { row = 9; column = 11 }; Move.Pass;
    Move.Place { row = 10; column = 9 }; Move.Pass;
    Move.Place { row = 11; column = 9 }; Move.Pass;
    Move.Place { row = 12; column = 10 }; Move.Pass;
    Move.Place { row = 12; column = 11 }; Move.Pass;
    Move.Place { row = 10; column = 12 }; Move.Pass;
    Move.Place { row = 11; column = 12 };
  ] in
  let state = play_moves state perimeter_moves in

  (* Black attempts to fill in the 2x2 square, which we expect triggers a self-capture error. *)
  let fill_moves = [
    Move.Place { row = 10; column = 10 }; Move.Pass;
    Move.Place { row = 10; column = 11 }; Move.Pass;
    Move.Place { row = 11; column = 10 }; Move.Pass;
    Move.Place { row = 11; column = 11 }
  ] in

  let rec try_moves state moves =
    match moves with
    | [] -> Ok state
    | m :: ms ->
      match Game_state.make_move state m with
      | Ok s -> try_moves s ms
      | Error e -> Error e
  in
  let result = try_moves state fill_moves in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Self_capture_violation) |}]

let%expect_test "Game_state.make_move: pass move" =
  let state = Game_state.create ~goal_captures:1 |> ok_exn in
  let stateAfterPass = Game_state.make_move state Move.Pass |> ok_exn_move in
  print_s [%sexp (stateAfterPass.decision : Decision.t)];
  [%expect {| (In_progress (whose_turn Black)) |}]
;;

(* 
let%expect_test "Game_state.make_move: Ko_violation error" =
  let state = Game_state.create ~goal_captures:5 |> ok_exn in
  (* Set up a simple Ko situation *)
  let moves =
    [ Move.Place { row = 0; column = 0 } (* White *)
    ; Move.Place { row = 0; column = 1 } (* Black *)
    ; Move.Place { row = 1; column = 0 } (* White *)
    ; Move.Place { row = 1; column = 1 } (* Black *)
    ; Move.Place { row = 0; column = 2 } (* White *)
    ; Move.Place { row = 2; column = 0 } (* Black *)
    ; Move.Place { row = 1; column = 2 } (* White *)
    ; Move.Place { row = 2; column = 1 } (* Black *)
    ]
  in
  let rec play_moves state moves =
    match moves with
    | [] -> state
    | m :: ms ->
      match Game_state.make_move state m with
      | Ok s -> play_moves s ms
      | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)])
  in
  let state = play_moves state moves in
  (* White tries to recapture at (1,1), which should trigger Ko violation *)
  let result = Game_state.make_move state (Move.Place { row = 1; column = 1 }) in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)];
  [%expect {| (Error Ko_violation) |}]
;;
*)