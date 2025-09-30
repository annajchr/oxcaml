
open! Core
open Hw2_capturego_logic

let max_score = Int.max_value
let min_score = Int.min_value

(* Heuristic: difference in captures, positive for Black, negative for White *)
let heuristic_value (game_state : Game_state.t) : int =
  match game_state.decision with
  | Decision.Stalemate -> 0
  | Decision.In_progress _ -> game_state.black_captures - game_state.white_captures
  | Decision.Winner player_kind ->
    (match player_kind with
     | Player_kind.Black -> max_score
     | Player_kind.White -> min_score)
;;

(* Generate and sort child states by heuristic value *)
let children (game_state : Game_state.t) ~(sort_by_whose_turn : Player_kind.t) : Game_state.t list =
  let compare =
    match sort_by_whose_turn with
    | Player_kind.Black -> Int.descending
    | Player_kind.White -> Int.ascending
  in
  let moves = Game_state.get_all_moves game_state in
  List.filter_map moves ~f:(fun move -> Game_state.make_move game_state move |> Result.ok)
  |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

(* Alpha-beta pruning search *)
let rec alpha_beta
    (game_state : Game_state.t)
    (depth : int)
    (alpha : int)
    (beta : int)
  : int =
  match game_state.decision with
  | Decision.In_progress { whose_turn } when depth > 0 ->
    let child_states = children game_state ~sort_by_whose_turn:whose_turn in
    (match whose_turn with
     | Player_kind.Black ->
       List.fold_until
         child_states
         ~init:(min_score, alpha)
         ~finish:(fun (value, _alpha) -> value)
         ~f:(fun (value, alpha) child ->
           let child_value = alpha_beta child (depth - 1) alpha beta in
           let value = Int.max value child_value in
           let alpha = Int.max alpha value in
           if value >= beta then Stop value else Continue (value, alpha))
     | Player_kind.White ->
       List.fold_until
         child_states
         ~init:(max_score, beta)
         ~finish:(fun (value, _beta) -> value)
         ~f:(fun (value, beta) child ->
           let child_value = alpha_beta child (depth - 1) alpha beta in
           let value = Int.min value child_value in
           let beta = Int.min beta value in
           if value <= alpha then Stop value else Continue (value, beta)))
  | _ -> heuristic_value game_state
;;

(* Find the best move for the current player using alpha-beta search *)
let best_move (game_state : Game_state.t) ~(depth : int) : Move.t option =
  match game_state.decision with
  | Decision.Winner _ | Decision.Stalemate -> None
  | Decision.In_progress { whose_turn } ->
    let moves = Game_state.get_all_moves game_state in
    let moves_and_children =
      List.filter_map moves ~f:(fun move ->
        Game_state.make_move game_state move
        |> Result.ok
        |> Option.map ~f:(fun child -> move, child))
    in
    let moves_and_values =
      List.map moves_and_children ~f:(fun (move, child) ->
        let value = alpha_beta child (depth - 1) min_score max_score in
        (move, value))
    in
    let best =
      match whose_turn with
      | Player_kind.Black -> List.max_elt moves_and_values ~compare:(fun (_, v1) (_, v2) -> Int.compare v1 v2)
      | Player_kind.White -> List.min_elt moves_and_values ~compare:(fun (_, v1) (_, v2) -> Int.compare v1 v2)
    in
    Option.map best ~f:fst
;;
