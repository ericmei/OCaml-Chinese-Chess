open Board
open Piece

(*
(* The color of AI's side; Red is true and Black if false *)
val col: bool ref
*)

(* compute the score based on the current board information*)
val evaluate: board->bool->int

(* evaluate the score *)
val eval_board: board-> bool -> int
