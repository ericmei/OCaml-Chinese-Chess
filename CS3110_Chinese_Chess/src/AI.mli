open Piece
open Board
open Move
open Score

(* Hashcode for a board's current state *)  
type board_hashcode

(* A record for the search result of a given state *)
type search_result

(* record previous AI computations in this game*)
type transposition_table

(* record history AI computations*)
type history_table

(* Determine AI's side color *)
val det_col: bool -> bool

(* The color of AI's side; Red is true and Black if false *)
val col: bool

(* sort the all possible moves*)
val sort: board-> step list

(* generate the best move using non-iterative alpha beta w/o tables *)
val best_move_v0: int-> board-> step list

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
val best_move: int-> board-> transposition_table-> history_table-> step list

(*update the historical information*)
val update: 
  board-> step-> transposition_table * history_table-> transposition_table * history_table

val easy_AI: 
  board-> transposition_table-> history_table-> step list * transposition_table * history_table
  
val hard_AI:
  board-> transposition_table-> history_table-> step list * transposition_table * history_table
