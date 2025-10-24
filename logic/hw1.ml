open! Core

(*
   Capture Go Game State Representation (HW1 Copy from Ocaml Playground linked in the slideshow)
*)

module Player_kind = struct
  type t =
    | Black
    | White
end

module Cell_position = struct
  type t =
    { row : int
    ; column : int
    }
end

module Stone = struct
  type t =
    { position : Cell_position.t
    ; owner : Player_kind.t
    }
end

module Move = struct
  type t =
    | Place of Cell_position.t
    | Pass
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Game_over of { winner : Player_kind.t }
    | Stalemate
  (*If no empty positions to move and goal captures still not met (this might not be possible?)*)
end

module Game_state = struct
  type t =
    { board : Stone.t list
    ; goal_captures : int
    ; black_captures : int
    ; white_captures : int
    ; decision : Decision.t
    }
end

let _initial_state : Game_state.t =
  { board = []
  ; goal_captures = 10
  ; black_captures = 0
  ; white_captures = 0
  ; decision = In_progress { whose_turn = White }
  }
;;

let _example_move : Move.t = Move.Place { row = 0; column = 0 }

let _example_game_state : Game_state.t =
  { board = [ { position = { row = 0; column = 0 }; owner = Player_kind.White } ]
  ; goal_captures = 10
  ; black_captures = 0
  ; white_captures = 0
  ; decision = Decision.In_progress { whose_turn = Player_kind.Black }
  }
;;

let _example_win_state : Game_state.t =
  { board =
      [ { position = { row = 0; column = 0 }; owner = Player_kind.White }
      ; { position = { row = 1; column = 0 }; owner = Player_kind.Black }
      ; { position = { row = 15; column = 15 }; owner = Player_kind.White }
      ; { position = { row = 0; column = 1 }; owner = Player_kind.Black }
      ]
  ; goal_captures = 1
  ; black_captures = 1
  ; white_captures = 0
  ; decision = Decision.Game_over { winner = Player_kind.Black }
  }
;;
