open! Core

module Player_kind = struct
  type t =
    | Black
    | White
  [@@deriving sexp, to_string, equal, compare]

  let opposite t =
    match t with
    | Black -> White
    | White -> Black
  ;;
end

module Cell_position = struct
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare, hash]

  (* Creates a [Cell_position.Map.t]. *)
  include functor Comparable.Make
end

module Stone = struct
  type t =
    { position : Cell_position.t
    ; owner : Player_kind.t
    }
  [@@deriving sexp, equal, compare]
end

module Move = struct
  type t =
    | Place of Cell_position.t
    | Pass
  [@@deriving sexp, equal]
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, equal]

  let is_game_over t =
    match t with
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  module Board_state = struct
    type t = Stone.t list [@@deriving sexp, compare]

    let of_board (board : Stone.t option array array) : t =
      Array.fold board ~init:[] ~f:(fun acc row ->
        Array.fold row ~init:acc ~f:(fun acc cell ->
          match cell with
          | Some stone -> stone :: acc
          | None -> acc))
      |> List.rev
    ;;
  end

  module Board_set = Set.Make (Board_state)

  type t =
    { board : Stone.t option array array (* 19x19 board *)
    ; previous_states : Board_set.t
    ; goal_captures : int
    ; black_captures : int
    ; white_captures : int
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp, equal]

  module Create_error = struct
    type t = Goal_captures_less_than_one [@@deriving sexp]
  end

  let create ~goal_captures : (t, Create_error.t list) Result.t =
    if goal_captures < 1
    then Error [ Create_error.Goal_captures_less_than_one ]
    else
      Ok
        { board = Array.make_matrix ~dimx:19 ~dimy:19 None
        ; previous_states = Board_set.empty
        ; goal_captures
        ; black_captures = 0
        ; white_captures = 0
        ; decision = Decision.In_progress { whose_turn = Player_kind.White }
        ; last_move = None
        }
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
      | Ko_violation
    [@@deriving sexp]
  end

  (* Helper Functions for handling dfs for capturing stones. *)
  let directions = [ -1, 0; 1, 0; 0, -1; 0, 1 ]
  let in_bounds row col = row >= 0 && row < 19 && col >= 0 && col < 19

  let get_stone_group (board : Stone.t option array array) (stone_pos : Cell_position.t) =
    match board.(stone_pos.Cell_position.row).(stone_pos.column) with
    | None -> [], true
    | Some (stone : Stone.t) ->
      let visited = Hash_set.create (module Cell_position) in
      let rec find_group_and_liberty (stone : Stone.t) group to_search =
        match to_search with
        (* If there is no neighbor left to search, we return the complete group
           and false to indicate no liberty was found. *)
        | [] -> group, false
        | hd :: tl when Hash_set.mem visited hd -> find_group_and_liberty stone group tl
        | hd :: tl ->
          Hash_set.add visited hd;
          (match board.(hd.row).(hd.column) with
           (* If we ever find an empty cell adjacent to the group, that means we have
             found at least one liberty, and can immediately return true knowing
            this group has not been captured. *)
           | None -> group, true
           | Some (s : Stone.t) ->
             (* We only add stones to the group of the same group owner. *)
             if Player_kind.equal s.owner stone.owner
             then (
               let neighbors =
                 List.filter_map directions ~f:(fun (dr, dc) ->
                   let r, c = hd.row + dr, hd.column + dc in
                   if in_bounds r c
                   then Some { Cell_position.row = r; column = c }
                   else None)
               in
               (* Add current stone to the group and the stone's neighbors to the positions left to search list. *)
               find_group_and_liberty stone (hd :: group) (neighbors @ tl)
               (* If the stone is owned by the opposite player, we skip that stone. *))
             else find_group_and_liberty stone group tl)
      in
      find_group_and_liberty stone [] [ stone_pos ]
  ;;

  let neighbors_of pos =
    List.filter_map directions ~f:(fun (dr, dc) ->
      let r, c = pos.Cell_position.row + dr, pos.column + dc in
      if in_bounds r c then Some { Cell_position.row = r; column = c } else None)
  ;;

  let remove_captured_stones board player pos : Stone.t option array array * int =
    let opponent = Player_kind.opposite player in
    let board_copy = Array.map ~f:Array.copy board in
    let captured_positions = ref [] in
    let checked = Hash_set.create (module Cell_position) in
    let neighbor_positions = neighbors_of pos in
    List.iter neighbor_positions ~f:(fun neighbor_pos ->
      match board.(neighbor_pos.row).(neighbor_pos.column) with
      (* If a neighbor stone is an opponent stone, we check if the current move
        has taken that group's last liberty. *)
      | Some (stone : Stone.t) when Player_kind.equal stone.owner opponent ->
        if not (Hash_set.mem checked neighbor_pos)
        then (
          let group, has_liberty = get_stone_group board neighbor_pos in
          (* After returning a group, its possible we will have already search the group of stones containing
           another neighbor. To prevent duplicate work, we mark any visited neighbors in the current
           search as 'checked'. *)
          List.iter group ~f:(fun p ->
            if List.mem neighbor_positions p ~equal:Cell_position.equal
            then Hash_set.add checked p);
          if not has_liberty then captured_positions := group @ !captured_positions)
      (* If neighbor is the player's stone, or empty, no check is necessary.
          TODO: add self-capture check. *)
      | _ -> ());
    (* Returns the updated board after removing captured stones. *)
    List.iter !captured_positions ~f:(fun p -> board_copy.(p.row).(p.column) <- None);
    board_copy, List.length !captured_positions
  ;;

  let check_winner t =
    match t.black_captures >= t.goal_captures, t.white_captures >= t.goal_captures with
    | true, _ -> Some Player_kind.Black
    | false, true -> Some Player_kind.White
    | false, false -> None
  ;;

  let check_positonal_ko
      (board : Stone.t option array array)
      (previous_states : Board_set.t)
      : bool
    =
    let board_list = Board_state.of_board board in
    Set.mem previous_states board_list
  ;;

  (* TODO: Add Self-Capture Error check (prevent player from making move that
       would result in their own stone being captured). *)
  let validate_move (t : t) (whose_turn : Player_kind.t) (move : Move.t)
      : Move_error.t option * Stone.t option array array
    =
    match move with
    | Move.Pass -> None, t.board
    | Move.Place pos ->
      if pos.row < 0 || pos.row >= 19 || pos.column < 0 || pos.column >= 19
      then Some Move_error.Illegal_cell_position, t.board
      else if Option.is_some t.board.(pos.row).(pos.column)
      then Some Move_error.Space_already_filled, t.board
      else (
        let new_board = Array.map ~f:Array.copy t.board in
        new_board.(pos.row).(pos.column)
        <- Some { Stone.position = pos; owner = whose_turn };
        if check_positonal_ko new_board t.previous_states
        then Some Move_error.Ko_violation, t.board
        else None, new_board)
  ;;

  let make_move (t : t) (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Decision.Winner _ | Decision.Stalemate -> Error Move_error.Game_is_over
    | Decision.In_progress { whose_turn } ->
      let error, new_board = validate_move t whose_turn move in
      (match error, new_board, move with
       | Some err, _, _ -> Error err
       | _, _, Move.Pass ->
         Ok
           { t with
             decision =
               Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }
           ; last_move = Some Move.Pass
           }
       | _, new_board, Move.Place pos ->
         let board_after_capture, captured_count =
           remove_captured_stones new_board whose_turn pos
         in
         let black_captures, white_captures =
           match whose_turn with
           | Player_kind.Black -> t.black_captures + captured_count, t.white_captures
           | Player_kind.White -> t.black_captures, t.white_captures + captured_count
         in
         let new_previous_states =
           Set.add t.previous_states (Board_state.of_board board_after_capture)
         in
         let winner = check_winner t in
         let decision =
           match winner with
           | Some p -> Decision.Winner p
           | None -> Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }
         in
         Ok
           { t with
             board = board_after_capture
           ; previous_states = new_previous_states
           ; black_captures
           ; white_captures
           ; decision
           ; last_move = Some move
           })
  ;;
end
