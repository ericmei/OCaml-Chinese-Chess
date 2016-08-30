open Board
open Piece
open Score
open Array


let init_board  = Board.init()

let print_piece (p : piece option) : unit =
  match  p with
  | None -> Printf.printf "%s" " "
  (* red // green *)
  | Some pc -> let clr = if pc.team then "\027[31m" else "\032[31m" in
    Printf.printf "%s %s" clr pc.print_name

(*
let print_board (b: board) : unit =
  Array.iter (fun inner_arr -> Array.iter print_piece inner_arr ; print_endline "") b
*)
