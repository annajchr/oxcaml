open! Core
open Capturego_logic_library
open Hw2_capturego_logic
(* open Hw4_alpha_beta_search *)

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
  Stdio.printf "Goal captures: %d\n" state.goal_captures
;;

let rec play_moves state moves =
  match moves with
  | [] -> state
  | m :: ms ->
    (match Game_state.make_move state m with
     | Ok s -> play_moves s ms
     | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)]))
;;

let pretty_decision_print (move : Move.t option) =
  match move with
  | None -> print_endline "AI chooses: None"
  | Some Move.Pass -> print_endline "AI chooses: Pass"
  | Some (Move.Place { row; column }) ->
    Printf.printf "AI chooses: Place stone at row %d, column %d\n" row column
;;

let%expect_test "alpha_beta chooses correct move for simple capture" =
  let state = Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn in
  let state =
    play_moves
      state
      [ Move.Place { row = 0; column = 0 } (* White *)
      ; Move.Place { row = 1; column = 0 } (* Black *)
      ; Move.Place { row = 0; column = 1 } (* White *)
      ; Move.Place { row = 1; column = 1 } (* Black *)
      ; Move.Place { row = 10; column = 2 } (* White *)
      ]
  in
  (* Black to move: can capture at (0,2) *)
  let move = Hw4_alpha_beta_search.best_move state ~depth:1 in
  pretty_print_board state;
  pretty_decision_print move;
  [%expect
    {|
    W W . . . . . . . . . . . . . . . . .
    B B . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . W . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn Black))
    Black captures: 0
    White captures: 0
    Goal captures: 1
    AI chooses: Place stone at row 0, column 2 |}]
;;


   let%expect_test "AI vs AI: first to 1 capture" =
  let state = Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn in
  let max_turns = 50 in
  let rec ai_vs_ai state depth turn_count =
    pretty_print_board state;
    if turn_count >= max_turns then state
    else match state.decision with
    | Decision.Winner _ | Decision.Stalemate -> state
    | Decision.In_progress _ ->
      let move = Hw4_alpha_beta_search.best_move state ~depth in
      match move with
      | None -> state
      | Some m ->
        match Game_state.make_move state m with
        | Ok next_state -> ai_vs_ai next_state depth (turn_count + 1)
        | Error e -> failwith (Sexp.to_string [%sexp (e : Game_state.Move_error.t)])
  in
  let _final_state = ai_vs_ai state 1 0 in
  [%expect {|
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
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn White))
    Black captures: 0
    White captures: 0
    Goal captures: 1
    W . . . . . . . . . . . . . . . . . .
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
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn Black))
    Black captures: 0
    White captures: 0
    Goal captures: 1
    W B . . . . . . . . . . . . . . . . .
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
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn White))
    Black captures: 0
    White captures: 0
    Goal captures: 1
    W B W . . . . . . . . . . . . . . . .
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
    . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . .
    (In_progress (whose_turn Black))
    Black captures: 0
    White captures: 0
    Goal captures: 1
    . B W . . . . . . . . . . . . . . . .
    B . . . . . . . . . . . . . . . . . .
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
    . . . . . . . . . . . . . . . . . . .
    (Winner Black)
    Black captures: 1
    White captures: 0
    Goal captures: 1 |}]
;;


(*
let%expect_test "alpha_beta chooses pass when no moves" =
  let state = Game_state.create ~goal_captures:1 |> Result.ok |> Option.value_exn in
  let state = play_moves state [
    Move.Place { row = 0; column = 0 }; (* White *)
    Move.Place { row = 1; column = 0 }; (* Black *)
    Move.Place { row = 0; column = 1 }; (* White *)
    Move.Place { row = 1; column = 1 }; (* Black *)
    Move.Place { row = 0; column = 2 }; (* White *)
    Move.Place { row = 1; column = 2 }; (* Black *)
    (* Board is filling up, test pass move *)
  ] in
  let move = best_move state ~depth:2 in
  print_s [%message "AI chooses move" (move : Move.t option)];
  [%expect {| (AI chooses move (Pass)) |}]
;; *)
