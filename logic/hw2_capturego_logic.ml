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
  [@@deriving sexp, compare]

  (* Creates a [Cell_position.Map.t]. *)
  include functor Comparable.Make
end

module Stone = struct
  type t = {
    position : Cell_position.t;
    owner : Player_kind.t;
  } [@@deriving sexp, equal, compare]
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
  module Board_set = Set.Make(struct
    type t = Stone.t list [@@deriving sexp, compare]
  end)

  type t = {
    board : Stone.t list;
    previous_states : Board_set.t;
    goal_captures : int;
    black_captures : int;
    white_captures : int;
    decision : Decision.t;
    last_move : Move.t option;
  } [@@deriving sexp, equal]

  module Create_error = struct
    type t =
      | Goal_captures_less_than_one
    [@@deriving sexp]
  end

  let create ~goal_captures : (t, Create_error.t list) Result.t =
    if goal_captures < 1 then Error [Create_error.Goal_captures_less_than_one]
    else Ok {
      board = [];
      previous_states = Board_set.empty;
      goal_captures;
      black_captures = 0;
      white_captures = 0;
      decision = Decision.In_progress { whose_turn = Player_kind.White };
      last_move = None;
    }

  module Move_error = struct
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
      | Ko_violation
    [@@deriving sexp]
  end

  let check_winner t =
    match t.black_captures >= t.goal_captures, t.white_captures >= t.goal_captures with
    | true, _ -> Some Player_kind.Black
    | false, true -> Some Player_kind.White
    | false, false -> None
  ;;

  let check_positonal_ko (board : Stone.t list) (previous_states : Board_set.t) : bool =
    Set.mem previous_states board
  ;;

  let make_move (t : t) (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Decision.Winner _ | Decision.Stalemate -> Error Move_error.Game_is_over
    | Decision.In_progress { whose_turn } ->
      match move with
      | Move.Pass ->
        Ok { t with decision = Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }; last_move = Some Move.Pass }
      | Move.Place pos ->
        if List.exists t.board ~f:(fun stone -> Cell_position.equal stone.position pos) then Error Move_error.Space_already_filled
        else if pos.row < 0 || pos.row >= 19 || pos.column < 0 || pos.column >= 19 then Error Move_error.Illegal_cell_position
        else if check_positonal_ko t.board t.previous_states then Error Move_error.Ko_violation
        else
          let new_stone = { Stone.position = pos; owner = whose_turn } in
          let new_board = new_stone :: t.board in
          let new_previous_states = Set.add t.previous_states new_board in
          let black_captures = t.black_captures in
          let white_captures = t.white_captures in
          let goal_captures = t.goal_captures in
          let winner = check_winner t in
          let decision =
            match winner with
            | Some p -> Decision.Winner p
            | None -> Decision.In_progress { whose_turn = Player_kind.opposite whose_turn }
          in
          Ok {
            board = new_board;
            previous_states = new_previous_states;
            goal_captures;
            black_captures;
            white_captures;
            decision;
            last_move = Some move;
          }
end