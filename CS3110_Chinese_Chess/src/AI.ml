open Piece
open Board
open Move
open Score
exception TODO
  
type board_hashcode = string

type search_result = {depth:int; best:step list; conclu:int}

type transposition_table = (string, search_result) Hashtbl.t

type history_table = (string, search_result) Hashtbl.

(* Determine AI's color, assuming false is for red in input b *)
let det_col b : bool = 
	if b then true else false

(* initialize AI's color to black *)
let col = true

let sort = raise TODO

let generate_all_moves (b:board) (p:prev_step) (side:round): step list= 
	let all_pieces = get_alive_side b side in
	let each_steps = List.map (fun a -> generate_piece_move b p a) all_pieces in
	List.fold_left 
		(fun l s -> match s with
		 | None -> l
		 | Some t -> l@t)
		[] each_steps

let alphaBetaMax (alpha:int ref) (beta:int ref) (depth_left:int) 
	(b:board) (p:prev_step): int = 
	
	let result = ref (int_of_float neg_infinity) in
	if depth_left = 0 then (evaluate b)
	else 
		let all_moves = Array.of_list (generate_all_moves b p col) in
		let i = ref 0 in
		while ((!i <= (Array.length all_moves)) && (!beta <> !result)) 
		do 
			let updated_b = 
			let score = alphaBetaMin alpha beta (depth_left - 1) in
			if (score >= !beta) then result := !beta; i:=!i+1
			else if (score > !alpha) then alpha := score; i:=!i+1
			else i:=!i+1
		done;
		if !result = !beta then !beta else !alpha

and alphaBetaMin (alpha:int ref) (beta:int ref) (depth_left:int) 
	(b:board) (p:prev_step): int =

	let result = ref (int_of_float infinity) in
	if depth_left = 0 then -(evaluate b)
	else
		let all_moves = Array.of_list (generate_all_moves b p !col) in
		let i = ref 0 in
		while ((!i <= (Array.length all_moves)) && (!alpha <> !result)) 
		do
			let score = alphaBetaMax alpha beta (depth_left - 1) in
			if (score <= !alpha) then result := !alpha; i:=!i+1
			else if (score < !beta) then beta := score; i:=!i+1
			else i:=!i+1
		done;
		if !result = !alpha then !alpha else !beta

let best_move_v0 (n:int) (b:board) : step list = 
	(* need to modify the alpha beta to keep track of the optimum steps *)
	raise TODO

