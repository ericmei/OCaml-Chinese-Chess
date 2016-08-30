open AI
open Piece
open Board
open Move
open Score


let generate_all_moves (b:board) (p:prev_step) (side:bool): step list=
	let all_pieces = get_alive_side b side in
	let each_steps = List.map
		(fun a -> let l = generate_piece_move b p a in List.iter print_step l; l) all_pieces in
	List.flatten each_steps

let b0 = init () in
print_board b0;
let prev0 = init_PrevStep () in
(*
let moves = generate_all_moves b0 prev0 true in
List.iter print_step moves
*)
(*
let can_moves_list = generate_piece_move b0 prev0 canR1 in
let can_moves = Array.of_list can_moves_list in
let i = ref 0 in
while (!i < (Array.length can_moves))
do
	let (updated_b,updated_prev) = update_unmutable can_moves.(!i) b0 prev0 in
	let new_score = eval_board updated_b in
	Printf.printf "This step is: \n";
	print_step can_moves.(!i);
	print_board updated_b;
	Printf.printf "It has score %d \n" new_score;
	i := !i + 1
done;
*)
(*
let t=Unix.gettimeofday () in
let (score, pred) = best_move_v0 2 b0 prev0 true in
print_float (Unix.gettimeofday () -. t);
*)
(*
let score=quiescence min_int max_int 6 b0 prev0 true true in
print_int score
*)
(*
let all_moves = generate_all_moves b0 prev0 round in
let cap_moves = List.filter (fun s->
  begin
	match s.piece_captured with
	|None -> false
	|Some p-> true
  end
) all_moves in
List.iter print_step cap_moves;
print_endline "hihi";
let result=ref [] in
List.iter (fun s-> begin
	match s.piece_captured with
	|None -> ()
	|Some p-> result:=(s::(!result))
  end
) all_moves;
List.iter print_step !result
*)

let (score, pred) = best_move_v0 2 b0 prev0 true in
print_int score;
List.iter print_step pred;
print_int !cnt
