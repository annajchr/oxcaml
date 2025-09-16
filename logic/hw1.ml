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

let initial_state : game_state =
  { board = []
  ; goal_captures = 10
  ; black_captures = 0
  ; white_captures = 0
  ; decision = In_progress { whose_turn = White }
  }
;;

let example_move : move = Place { row = 0; column = 0 }

let example_game_state : game_state =
  { board = [{ position = { row = 0; column = 0 }; owner = White }]
  ; goal_captures = 10
  ; black_captures = 0
  ; white_captures = 0
  ; decision = In_progress { whose_turn = Black }
  }
;;

let example_win_state : game_state =
  { board = [{ position = { row = 0; column = 0 }; owner = White }
            ; {position = { row = 1; column = 0 }; owner = Black }
            ; {position = { row = 15; column = 15 }; owner = White }
            ; {position = { row = 0; column = 1 }; owner = Black }
            ]
  ; goal_captures = 1
  ; black_captures = 1
  ; white_captures = 0
  ; decision = Winner Black
  }
;;
