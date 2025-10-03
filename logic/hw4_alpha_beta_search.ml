open! Core
open Hw2_capturego_logic

(*Heuristic Helper: Count groups owned by a player with only 1 liberty (atari) *)
let count_atari_groups (game_state : Game_state.t) (player : Player_kind.t) : int =
  let module CP = Cell_position in
  let seen =
    Core.Hash_set.create
      ~size:1000
      (module struct
        include CP

        let hash t = Hashtbl.hash t
      end)
  in
  let atari_count = ref 0 in
  let board = game_state.board in
  for row = 0 to 18 do
    for col = 0 to 18 do
      match board.(row).(col) with
      | Some stone when Player_kind.equal stone.owner player ->
        if not (Core.Hash_set.mem seen stone.position)
        then (
          let group, _ = Game_state.get_stone_group board stone.position in
          Core.List.iter group ~f:(fun pos -> Core.Hash_set.add seen pos);
          let group_libs =
            Core.List.fold group ~init:0 ~f:(fun acc pos ->
              let libs =
                Game_state.neighbors_of pos
                |> Core.List.count ~f:(fun npos ->
                  let r = npos.CP.row in
                  let c = npos.CP.column in
                  Option.is_none board.(r).(c))
              in
              acc + libs)
          in
          if group_libs = 1 then atari_count := !atari_count + 1)
      | _ -> ()
    done
  done;
  !atari_count
;;

(*Heuristic Helper: count liberties for all stones of a player *)
let total_liberties (game_state : Game_state.t) (player : Player_kind.t) : int =
  let module CP = Cell_position in
  let seen =
    Hash_set.create
      (module struct
        include CP

        let hash t = Hashtbl.hash t
      end)
  in
  let liberties = ref 0 in
  for row = 0 to 18 do
    for col = 0 to 18 do
      match game_state.board.(row).(col) with
      | Some stone when Player_kind.equal stone.owner player ->
        if not (Hash_set.mem seen stone.position)
        then (
          let group, _ = Game_state.get_stone_group game_state.board stone.position in
          List.iter group ~f:(fun pos -> Hash_set.add seen pos);
          let group_libs =
            List.sum
              (module Int)
              group
              ~f:(fun pos ->
                Game_state.neighbors_of pos
                |> List.count ~f:(fun npos ->
                  let r = npos.CP.row in
                  let c = npos.CP.column in
                  Option.is_none game_state.board.(r).(c)))
          in
          liberties := !liberties + group_libs)
      | _ -> ()
    done
  done;
  !liberties
;;

(*Heuristic Helper: count stones in long lines (horizontal/vertical) *)
let count_long_lines (game_state : Game_state.t) (player : Player_kind.t) : int =
  let board = game_state.board in
  let count_line dir =
    let count = ref 0 in
    for i = 0 to 18 do
      let line =
        match dir with
        | `Row -> Array.to_list board.(i)
        | `Col -> List.init 19 ~f:(fun j -> board.(j).(i))
      in
      let streak = ref 0 in
      List.iter line ~f:(function
        | Some stone when Player_kind.equal stone.owner player ->
          streak := !streak + 1;
          if !streak >= 3 then count := !count + 1
        | _ -> streak := 0)
    done;
    !count
  in
  count_line `Row + count_line `Col
;;

(*Heuristic Helper: count compact stones (with 2+ friendly neighbors) *)
let count_compact_stones (game_state : Game_state.t) (player : Player_kind.t) : int =
  let module CP = Cell_position in
  let board = game_state.board in
  let count = ref 0 in
  for row = 0 to 18 do
    for col = 0 to 18 do
      match board.(row).(col) with
      | Some stone when Player_kind.equal stone.owner player ->
        let neighbors = Game_state.neighbors_of stone.position in
        let friendly =
          List.count neighbors ~f:(fun npos ->
            let r = npos.CP.row in
            let c = npos.CP.column in
            match board.(r).(c) with
            | Some s when Player_kind.equal s.owner player -> true
            | _ -> false)
        in
        if friendly >= 2 then count := !count + 1
      | _ -> ()
    done
  done;
  !count
;;

(*Heuristic Helper: get groups of stones owned by player. *)
let count_player_groups (game_state : Game_state.t) (player : Player_kind.t) : int =
  let module CP = Cell_position in
  let board = game_state.board in
  let seen =
    Hash_set.create
      (module struct
        include CP

        let hash t = Hashtbl.hash t
      end)
  in
  let group_count = ref 0 in
  for row = 0 to 18 do
    for col = 0 to 18 do
      match board.(row).(col) with
      | Some stone when Player_kind.equal stone.owner player ->
        if not (Hash_set.mem seen stone.position)
        then (
          let group, _ = Game_state.get_stone_group board stone.position in
          List.iter group ~f:(fun pos -> Hash_set.add seen pos);
          group_count := !group_count + 1)
      | _ -> ()
    done
  done;
  !group_count
;;

let heuristic_value (game_state : Game_state.t) : int =
  match game_state.decision with
  | Decision.Stalemate -> 0
  | Decision.Winner _ ->
    (* Assign a high positive or negative value based on the winner *)
    (match game_state.decision with
     | Decision.Winner Player_kind.Black -> Int.max_value
     | Decision.Winner Player_kind.White -> Int.min_value
     | _ -> 0)
    (* This case won't occur *)
  | Decision.In_progress { whose_turn } ->
    let self_libs = total_liberties game_state whose_turn in
    let opp_libs = total_liberties game_state (Player_kind.opposite whose_turn) in
    let self_captures, opp_captures =
      match whose_turn with
      | Player_kind.Black -> game_state.black_captures, game_state.white_captures
      | Player_kind.White -> game_state.white_captures, game_state.black_captures
    in
    let self_atari = count_atari_groups game_state whose_turn in
    let opp_atari = count_atari_groups game_state (Player_kind.opposite whose_turn) in
    let long_lines = count_long_lines game_state whose_turn in
    let compact = count_compact_stones game_state whose_turn in
    let group_count = count_player_groups game_state whose_turn in
    let filled_cells =
      Array.fold game_state.board ~init:0 ~f:(fun acc row ->
        acc + Array.count row ~f:Option.is_some)
    in
    let board_size = 19 * 19 in
    let percent_filled = Float.of_int filled_cells /. Float.of_int board_size in
    let weights =
      if Float.(percent_filled < 0.33)
      then (* Early game *)
        ( 1000
        , (* captures *)
          500
        , (* compact groups *)
          600
        , (* group count *)
          0
        , (* liberties *)
          40
        , (* opponent atari *)
          -1000
        , (* self atari *)
          -80 )
        (* long lines *)
      else if Float.(percent_filled < 0.66)
      then (* Mid game *)
        ( 1000
        , (* captures *)
          200
        , (* compact groups *)
          200
        , (* group count *)
          500
        , (* liberties *)
          520
        , (* opponent atari *)
          -1000
        , (* self atari *)
          -100 )
        (* long lines *)
      else (* Late game *)
        ( 1500
        , (* captures *)
          100
        , (* compact groups *)
          300
        , (* group count *)
          200
        , (* liberties *)
          200
        , (* opponent atari *)
          -1000
        , (* self atari *)
          -80 )
      (* long lines *)
    in
    let w_captures, w_compact, w_groups, w_libs, w_atari, w_self_atari, w_lines =
      weights
    in
    (w_captures * (self_captures - opp_captures))
    + (w_compact * compact)
    + (w_groups * group_count)
    + (w_libs * (self_libs - opp_libs))
    + (w_atari * opp_atari)
    + (w_self_atari * self_atari)
    + (w_lines * long_lines)
;;

let is_capture (parent : Game_state.t) (child : Game_state.t) (whose_turn : Player_kind.t)
  : bool
  =
  match whose_turn with
  | Player_kind.Black -> child.black_captures > parent.black_captures
  | Player_kind.White -> child.white_captures > parent.white_captures
;;

let best_move (game_state : Game_state.t) : Move.t option =
  match game_state.decision with
  | Decision.Winner _ | Decision.Stalemate -> None
  | Decision.In_progress { whose_turn } ->
    let filled_cells =
      Array.fold game_state.board ~init:0 ~f:(fun acc row ->
        acc + Array.count row ~f:Option.is_some)
    in
    if filled_cells = 0
    then Some (Move.Place { Cell_position.row = 9; column = 9 })
    else (
      let moves = Game_state.get_all_moves game_state in
      let moves_and_children =
        List.filter_map moves ~f:(fun move ->
          Game_state.make_move game_state move
          |> Result.ok
          |> Option.map ~f:(fun child -> move, child))
      in
      (* If any moves resulted in a capture, prioritize those moves over any other heuristic. *)
      let priority_moves =
        List.filter moves_and_children ~f:(fun (_move, child) ->
          is_capture game_state child whose_turn)
      in
      let candidate_moves =
        if not (List.is_empty priority_moves) then priority_moves else moves_and_children
      in
      let moves_and_values =
        List.map candidate_moves ~f:(fun (move, child) ->
          let value = heuristic_value child in
          move, value)
      in
      let best_score =
        List.max_elt moves_and_values ~compare:(fun (_, v1) (_, v2) -> Int.compare v1 v2)
        |> Option.map ~f:snd
      in
      match best_score with
      | None -> None
      | Some score ->
        let best_moves =
          List.filter moves_and_values ~f:(fun (_move, value) -> value = score)
        in
        (match best_moves with
         | [] -> None
         | lst ->
           let len = List.length lst in
           let idx = Random.int len in
           Some (fst (List.nth_exn lst idx))))
;;

(* Alpha-beta pruning search. *)
(* TODO: Right now Alpha Beta Pruning is unused since attempted to go depth > 1 led to
   extremely long runtimes, even at depth =2. Currently, the best_move function uses the
  heuristic to maximize next move based on direct next board states.  *)

let _children (game_state : Game_state.t) ~(sort_by_whose_turn : Player_kind.t)
  : Game_state.t list
  =
  let compare =
    match sort_by_whose_turn with
    | Player_kind.Black -> Int.descending
    | Player_kind.White -> Int.ascending
  in
  let moves = Game_state.get_all_moves game_state in
  List.filter_map moves ~f:(fun move -> Game_state.make_move game_state move |> Result.ok)
  |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

let rec _alpha_beta (game_state : Game_state.t) (depth : int) (alpha : int) (beta : int)
  : int
  =
  match game_state.decision with
  | Decision.In_progress { whose_turn } when depth > 0 ->
    let child_states = _children game_state ~sort_by_whose_turn:whose_turn in
    (match whose_turn with
     | Player_kind.Black ->
       List.fold_until
         child_states
         ~init:(Int.min_value, alpha)
         ~finish:(fun (value, _alpha) -> value)
         ~f:(fun (value, alpha) child ->
           let child_value = _alpha_beta child (depth - 1) alpha beta in
           let value = Int.max value child_value in
           let alpha = Int.max alpha value in
           if value >= beta then Stop value else Continue (value, alpha))
     | Player_kind.White ->
       List.fold_until
         child_states
         ~init:(Int.max_value, beta)
         ~finish:(fun (value, _beta) -> value)
         ~f:(fun (value, beta) child ->
           let child_value = _alpha_beta child (depth - 1) alpha beta in
           let value = Int.min value child_value in
           let beta = Int.min beta value in
           if value <= alpha then Stop value else Continue (value, beta)))
  | _ -> heuristic_value game_state
;;
