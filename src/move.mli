open Piece
open Board

(* the following type tells one step during one game*)
type step={start:position; destination: position; piece_captured: piece option}

val init_step : unit -> step
(* record the previous step*)
type prev_step

val in_bound: position -> bool

val init_PrevStep: unit -> prev_step

(*check whether a step is valid or not*)
val check_valid: board->prev_step->step->bool

(* check whether game is over after this step*)
val check_win: board->prev_step->step->bool

(* check whether either side is checked*)
val checked: board-> prev_step -> bool ->bool

(* update the game information*)
val update_unmutable: step->board->prev_step->board*prev_step

val update_board : board -> step -> unit
(* update a previous step with a new step*)
val update_prev : step -> prev_step -> prev_step

(*generate all possible move steps for one piece *)
val generate_piece_move: board-> prev_step-> piece-> step list

(*generate all possible move steps for one side *)
val generate_all_moves: board-> prev_step-> bool-> step list

(* print for debugging purpose*)
val print_step: step->unit

val undo_one : board -> prev_step -> prev_step

val undo : board -> prev_step -> prev_step
