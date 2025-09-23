open! Core

module Player_kind : sig
  type t =
    | Black
    | White
  [@@deriving sexp, to_string, equal, compare]

  val opposite : t -> t
end

module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare]

  (* Defines a [Cell_position.Map.t]. *)
  include Comparable.S with type t := t
end

module Stone : sig
  type t =
    { position : Cell_position.t
    ; owner : Player_kind.t
    }
  [@@deriving sexp, equal, compare]
end

module Move : sig
  type t =
    | Place of Cell_position.t
    | Pass
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  module Board_set : Set.S with type Elt.t = Stone.t list

  type t =
    { board : Stone.t option array array (** 19x19 board *)
    ; previous_states : Board_set.t
    ; goal_captures : int
    ; black_captures : int
    ; white_captures : int
    ; decision : Decision.t
    ; last_move : Move.t option (** For animation purposes. *)
    }
  [@@deriving sexp, equal]

  module Create_error : sig
    type t = Goal_captures_less_than_one [@@deriving sexp]
  end

  val create : goal_captures:int -> (t, Create_error.t list) Result.t

  module Move_error : sig
    type t =
      | Game_is_over
      | Space_already_filled
      | Illegal_cell_position
      | Ko_violation
      | Self_capture_violation
    [@@deriving sexp]
  end

  val get_all_moves : t -> Move.t list

  (* Atari Go/ Go rules include Ko condition; https://www.pandanet.co.jp/English/learning_go/learning_go_8.html.
  There are many variations of Simple-/Super- KO rules. For Atari Go (which is first-to-one capture), a KO rule is not
  necessary, however, since my capture go game will have variable number of first-to-x captures, I will add
  a KO Rule to prevent an infinite recaptures situation. 
  
  This function implements the Positional Super-KO rule, which states that a player may not make a move that would result
  in any previous board position.
  *)
  val check_positonal_ko : Stone.t option array array -> Board_set.t -> bool

  (* Validates the move against the possible Move errors. Returns an error if a move error is detected,
    otherwise returns the updated board with the current move stone on the board. *)
  val validate_move
    :  t
    -> Player_kind.t
    -> Move.t
    -> Move_error.t option * Stone.t option array array

  (* given the new board after the player makes a valid move, the function removes any captured stones
     and returns the number of stones captured, for scoring purposes. *)
  (* val remove_captured_stones : Stone.t list -> Player_kind.t -> Stone.t list * int *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  (* module For_testing : sig
    val all_directions : (int * int) list
  end *)
end
