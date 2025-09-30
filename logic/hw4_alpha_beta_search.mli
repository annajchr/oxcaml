
open! Core
open Hw2_capturego_logic

val alpha_beta : Game_state.t -> int -> int -> int -> int
val best_move : Game_state.t -> depth:int -> Move.t option
