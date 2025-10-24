open! Core

(*
   Capture Go Game State Representation (HW1 Copy from Ocaml Playground linked in the slideshow)
*)

module Player_kind : sig
  type t =
    | Black
    | White
end

module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
end

module Stone : sig
  type t =
    { position : Cell_position.t
    ; owner : Player_kind.t
    }
end

module Move : sig
  type t =
    | Place of Cell_position.t
    | Pass
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Game_over of { winner : Player_kind.t }
    | Stalemate
  (*If no empty positions to move and goal captures still not met (this might not be possible?)*)
end

module Game_state : sig
  type t =
    { board : Stone.t list
    ; goal_captures : int
    ; black_captures : int
    ; white_captures : int
    ; decision : Decision.t
    }
end
