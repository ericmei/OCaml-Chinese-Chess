open Piece

(*
 *board={first=piece array array; second=(string,piece) Hashtbl}
 *)
type board

(* the variable round tells the current round
 * is red side's turn or blue side's turn*)
val round: bool

(*get the piece given position*)
val check_position: board-> position-> piece option

(*the following function can tell whether a piece still exists on the borad*)
val check_alive: board->string->bool

(*get the position given a piece*)
val get_position: board->string-> position option

(*giving all the pieces which are still alive*)
val get_alive_pieces: board-> piece list

(*giving all the pieces on a specific side which are still alive*)
val get_alive_side: board -> round -> piece list

(* initialize the board*)
val init: unit-> board

(*print the ball*)
val print_board: board->unit

(*print the peice*)
val print_piece: piece->unit
