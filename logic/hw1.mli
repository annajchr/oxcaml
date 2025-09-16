open! Core

type player_kind =
  | Black
  | White


type cell_position =
  { row : int
  ; column : int
  }

type stone =
  { position : cell_position
  ; owner : player_kind
  }

type move = 
  | Place of cell_position
  | Pass

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind
  | Stalemate


type game_state =
  { board : stone list
  ; goal_captures : int
  ; black_captures : int
  ; white_captures : int
  ; decision : decision
  }

val initial_state : game_state
val example_move : move
val example_game_state : game_state
val example_win_state : game_state
