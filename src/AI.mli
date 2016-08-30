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

val cnt: int ref

(*Static search with quiescence algorithm; returns the score of a leaf*)
val quiescence: int->int->int->board->prev_step->bool->bool->int

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
val best_move_v0: int-> board-> prev_step-> bool-> int*step list

(* AI generating a random valid step*)
val random_AI:
  board-> prev_step -> bool -> step

(* easy level AI*)
val easy_AI:
  board-> prev_step -> bool -> step

(* hard level AI*)
val hard_AI:
  board-> prev_step -> bool -> step
