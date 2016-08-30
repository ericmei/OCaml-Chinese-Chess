open Piece
open Board
open Move
open Score
exception TODO

type board_hashcode = string

type search_result = {depth:int; best:step list; conclu:int}

type transposition_table = (string, search_result) Hashtbl.t

type history_table = (string, search_result) Hashtbl.t

let depth_limit=6

let nHistoryTable=Hashtbl.create (90*90)

let ()=
	for i = 1 to 9 do
	  for j=1 to 10 do
			for m=1 to 9 do
				for n=1 to 10 do
					Hashtbl.add nHistoryTable ((i,j),(m,n)) 0
				done
			done
		done
	done

(* The value of each piece type
*)
let mVV (p:piece) =
  match p.type_of with
  |General->5
  |Advisor->1
  |Elephant->1
  |Horse->3
  |Rook->4
  |Cannon->3
  |Soldier->2

let value_MVV (b:board) (s:step)=
  let pos=
  match s.piece_captured with
  |None->0
  |Some dst->(mVV dst)
  in
  begin
  match (check_position b s.start) with
   | None -> failwith "impossible"
   | Some src->pos-(mVV src)
  end

(* compare the MVV vals of two steps*)
let compared_MVV (b:board) (s1:step) (s2:step)=
  (value_MVV b s2)-(value_MVV b s1)

(* compare the vals of two steps in history table*)
let compared_hist s1 s2=
	(Hashtbl.find nHistoryTable (s2.start, s2.destination))
	-(Hashtbl.find nHistoryTable (s1.start, s1.destination))

(* Check if the game has ended given a current board *)
let check_end_game (b:board) (p:prev_step) : bool =
	(generate_all_moves b p true = []) || (generate_all_moves b p false = []) ||
	(not ((check_alive b "GR") && (check_alive b "GB")))


(* quiescence search, i.e. dynamic search *)
let rec quiescence (alpha:int) (beta:int) (depth:int)
(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool):int=
  if depth=0 then (eval_board b ai_col)
  else
  (
    if ai_col=curr_rd then
    (
    let v = ref alpha in
    let result = ref min_int in
    let sort_moves =
    if checked b (update_prev {start=(1,1);destination=(1,2);piece_captured=None} p)
    	(not curr_rd) then
      let all_moves = generate_all_moves b p curr_rd in
      List.sort (fun s1 s2->compared_MVV b s1 s2) all_moves
    else
    (
      let score=eval_board b ai_col in
      (if (score > !v) then
            v:= score
       else if (score > beta) then
            result := beta
      else ()
      );
      (
      if !result=beta then []
      else let all_moves = generate_all_moves b p curr_rd in
           let cap_moves = ref [] in
           let ()=List.iter (fun s-> begin
           match s.piece_captured with
           |None -> ()
           |Some p-> cap_moves:=(s::(!cap_moves))
           end
           ) all_moves in
           List.sort (fun s1 s2->compared_MVV b s1 s2) !cap_moves
      )
    ) in
    (
    if !result=beta then !result
    else
       (
        let i=ref 0 in
        let result = ref min_int in
        let sort_moves_array=Array.of_list sort_moves in
        while ((!i < (Array.length sort_moves_array)) && (beta <> !result))
        do
          let (updated_b,updated_prev) = update_unmutable sort_moves_array.(!i) b p in
   
          let next=(quiescence !v beta (depth-1) updated_b updated_prev
            ai_col (not curr_rd)) in
          (if (next > !v) then
            v:= next
          else if (next > beta) then
            result := beta
          else ()
          );
        i:=!i+1;
        done;
        if !result=beta then !result
        else !v
      )
    )
  )


  else
    (
    let v = ref beta in
    let result = ref max_int in
    let sort_moves =
    if checked b (init_PrevStep ()) (not curr_rd) then
      let all_moves = generate_all_moves b p curr_rd in
      List.sort (fun s1 s2->compared_MVV b s1 s2) all_moves
    else
    (
      let score=eval_board b ai_col in
      (if (score < !v) then
            v:= score
       else if (score < alpha) then
            result := alpha
      else ()
      );
      (
      if !result=alpha then []
      else let all_moves = generate_all_moves b p curr_rd in
           let cap_moves = ref [] in
           let ()=List.iter (fun s-> begin
           match s.piece_captured with
           |None -> ()
           |Some p-> cap_moves:=(s::(!cap_moves))
           end
           ) all_moves in
           List.sort (fun s1 s2->compared_MVV b s1 s2) !cap_moves
      )
    ) in
    (
    if !result=alpha then !result
    else
       (
        let i=ref 0 in
        let result = ref max_int in
        let sort_moves_array=Array.of_list sort_moves in
        while ((!i < (Array.length sort_moves_array)) && (alpha <> !result))
        do
          let (updated_b,updated_prev) = update_unmutable sort_moves_array.(!i) b p in
          let next=(quiescence alpha !v (depth-1) updated_b updated_prev
            ai_col (not curr_rd)) in
          (if (next < !v) then
            v:= next
          else if (next < alpha) then
            result := alpha
          else ()
          );
        i:=!i+1;
        done;
        if !result=alpha then !result
        else !v
      )
    )
  )
)

let cnt = ref 0

let rec alphaBeta (alpha:int) (beta:int) (depth_left:int)
	(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool): int*step list =

	if depth_left = 0 then (cnt := !cnt + 1; 
		((quiescence alpha beta depth_limit b p ai_col curr_rd), []))
	else if check_end_game b p then (cnt := !cnt + 1; ((eval_board b ai_col),[]))
	else

		(if curr_rd = ai_col then
			(let result = ref min_int in
			let v = ref alpha in
			let i = ref 0 in
			let best_steps = ref [] in
			begin match (generate_all_moves b p ai_col) with
			
			| [] -> (depth_left - max_int,[])
			| l ->
				List.iter (fun a -> if (in_bound a.start)&&(in_bound a.destination) then ()
					else print_step a) l;
				let sorted_l = List.sort compared_hist l in
				let all_moves = Array.of_list sorted_l in

				while ((!i < (Array.length all_moves)) && (beta <> !result))
				do

					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
			
          			let flag=ref false in
					(if (score > !v) then
						(
						v:= score;
 						best_steps:= all_moves.(!i)::new_best_steps;
						flag:=true
						)
					else if (score > beta) then
						(result := beta;
					  	flag:=true
					  	)
					else ()
					);

					(if (!flag=true) then
						let new_val=(Hashtbl.find nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination))+(depth_left*depth_left)
						in
						(Hashtbl.replace nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination)
					  new_val)
          			else ());
					cnt := !cnt + 1;
					i:= !i+1
				done;
				
				if !result = beta then (beta,!best_steps)
				else (!v,!best_steps)
			end)

		else
			(
			let result = ref max_int in
			let v = ref beta in
			let i = ref 0 in
			let best_steps = ref [] in
			begin match generate_all_moves b p (not ai_col) with
			
			| [] -> ((-depth_left) - min_int,[])
			| l ->
				List.iter (fun a -> if (in_bound a.start)&&(in_bound a.destination) then ()
					else print_step a) l;
				let sorted_l = List.sort compared_hist l in
				let all_moves = Array.of_list sorted_l in

				while ((!i < (Array.length all_moves)) && (alpha <> !result))
				do
					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
         
          			let flag=ref false in
					(
					if (score < !v) then
						(v:= score;
						best_steps:= all_moves.(!i)::new_best_steps;
						flag:=true
						)
					else if (score < alpha) then
					  (result := alpha;
					  flag:=true)
					else ());
					(if (!flag=true) then
						let new_val=(Hashtbl.find nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination))+(depth_left*depth_left)
						in
						(Hashtbl.replace nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination)
					  new_val)

					else ());
					cnt := !cnt + 1;
					i:= !i+1
				done;

				if !result = alpha then (alpha,!best_steps)
				else (!v,!best_steps)
			end)
		)



let best_move_v0 (n:int) (b:board) (p:prev_step) (ai_col:bool): int*step list =
	
	  let t=Unix.gettimeofday () in
		let result=ref (0, []) in
		let i=ref 1 in
		while ((Unix.gettimeofday () -. t)<=300.0 && !i<=n) do
			result:=(alphaBeta min_int max_int (!i) b p ai_col ai_col);
			i:=!i+1
		done;

		!result


let random_AI (b:board) (p:prev_step) (ai_col:bool): step =
    let all_moves=generate_all_moves b p ai_col in
		List.nth all_moves (Random.int (List.length all_moves))

let easy_AI (b:board) (p:prev_step) (ai_col:bool): step =
  let res =
  let (score,pred) = best_move_v0 1 b p ai_col in
  List.hd pred
  in let () = print_step res in res

let hard_AI (b:board) (p:prev_step) (ai_col:bool): step =
  let res =
  let (score,pred) = best_move_v0 4 b p ai_col in
  List.hd pred
  in let () = print_step res in res
