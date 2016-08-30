open Piece

(*
 *board={first=piece array array; second=(string,piece) Hashtbl}
 *)
type board

(*get the piece given position*)
val check_position: board-> position-> piece option

(*the following function can tell whether a piece still exists on the borad*)
val check_alive: board->string->bool

(*get the position given a piece*)
val get_position: board->string-> position option

(*giving all the pieces which are still alive*)
val get_alive_pieces: board-> piece list

(*giving all the pieces on a specific side which are still alive*)
val get_alive_side: board -> bool -> piece list

(* change the board*)
val change_entry: board-> position -> position-> piece option->unit

(*change one position to a piece (option)*)
val change_one : board -> position -> piece option -> unit

(* make a copy of board*)
val copy: board->board

(* initialize the board*)
val init: unit-> board

(* grabs the piece array from the board*)
val get_boardArray: board -> piece option array array


(*print the ball*)
val print_board: board->unit

(*print the peice*)
val print_piece: piece option->unit
