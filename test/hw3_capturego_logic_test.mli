open! Core
open Capturego_logic_library
open Hw2_capturego_logic

val ok_exn : ('a, 'b list) result -> 'a
val ok_exn_move : ('a, Game_state.Move_error.t) result -> 'a
val pretty_print_board : Game_state.t -> unit
val random_walk_capturego : random_seed:int -> unit