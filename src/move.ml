(* x is the column, y is the row*)
open Board

open Piece

exception TODO

exception InvalidMove

(* the following type tells one step during one game*)

type step={start:position; destination: position; piece_captured: piece option}

type prev_step = step list

let init_step = {start= (1,1); destination = (1,1) ; piece_captured = None}

let init_PrevStep () = []

let in_bound ((x,y):position) : bool=
  x>=1 && x<=9 && y>=1 && y <=10



let init_step () = {start = (1,1); destination = (1,1); piece_captured = None}

let in_square (pc:piece) ((x,y): position) : bool =
match pc.team with
  | true  -> x>=4 &&x<=6 && y>=1 && y<=3
  | false -> x>=4 && x <= 6 && y>=8 && y<=10





let self_side (pc:piece) ((x,y): position):bool=
if (pc.team=true && x>=1 && x<=9 && y>=1 && y <=5) then true
else if (pc.team=false && x>=1 && x<=9 && y>=6 && y <=10) then true
else false



let print_step stp =
  begin match stp with
  | {start = st; destination = ds; piece_captured=pcap} ->
  Printf.printf "starting position: (%d, %d)\n" (fst st) (snd st);
  Printf.printf "ending position: (%d, %d)\n" (fst ds) (snd ds);
  print_string "Piece captured: ";
  print_piece pcap;
  print_endline ""
  end





(*generate all the steps of a Rook on board with position p *)

let move_rook (b:board) (pc:piece) ((x,y): position) :step list =
begin
  let result = ref [] in
  let rec loop_forward curr_position =
  begin
  match  (check_position b curr_position), (in_bound curr_position) with

  | None, true -> result := {start = (x,y); destination = curr_position ;

      piece_captured = None}::(!result);

      loop_forward (fst curr_position, (snd curr_position)+1)

  | Some sth ,true -> if sth.team <> pc.team then

      result := {start = (x,y); destination = curr_position;

      piece_captured = Some sth}::(!result) else ()

  | _ , _ -> ()
  end
  in
  let rec loop_back curr_position =
  begin
  match (check_position b curr_position), (in_bound curr_position ) with
  | None, true-> result := {start = (x, y); destination = curr_position;
    piece_captured = None}::(!result); loop_back  (fst curr_position, (snd curr_position)-1)
  | Some sth ,true -> if sth.team <> pc.team then
      result := {start = (x, y); destination = curr_position;
    piece_captured = Some sth}::(!result)  else ()
  | _ , _ -> ()
  end
  in
  let rec loop_left  curr_position =
  begin
    match (check_position b curr_position), (in_bound curr_position ) with
    | None, true -> result:= {start = (x, y); destination = curr_position;
      piece_captured = None}::(!result); loop_left  ((fst curr_position)-1, snd curr_position)
    | Some sth ,true -> if sth.team <> pc.team then
         result := {start = (x, y); destination = curr_position;
      piece_captured = Some sth}::(!result) else ()
    | _ , _ -> ()
  end
  in
  let rec loop_right curr_position =
  begin
    match (check_position b curr_position), (in_bound curr_position )with
    | None, true -> result:= {start = (x, y); destination = curr_position;
      piece_captured = None}::(!result); loop_right  ((fst curr_position)+1, snd curr_position)
    | Some sth ,true -> if sth.team <> pc.team then
         result := {start = (x, y); destination = curr_position;
      piece_captured = Some sth}::(!result) else ()
   | _ , _ -> ()
  end
  in
  loop_forward (x, y+1);

  loop_back (x, y-1);

  loop_right (x+1 , y);

  loop_left (x-1, y);

  !result
end



 let catch_ok (b :board) (pc :piece) (p: position) : bool =
  match check_position b p with
  | None -> true
  | Some sth -> not (sth.team = pc.team)



(*move *)

let move_soldier (b:board) (pc:piece) ((x,y): position) : step list =
 (*red piece in its own part: row 0-5*)
match pc.team, y<=5 with
 (*red piece, in river side*)
 | true, true -> if (catch_ok b pc (x,y+1) ) then [{start= (x,y); destination = (x, y+1); piece_captured =
  (check_position b (x, y+1))}] else []
 (*red piece, other side of river*)
 | true, false -> let raw_pos = [(x+1, y); (x-1, y); (x, y+1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match check_position b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_pos )
 (*black piece, own side*)
 | false, false -> if (catch_ok b pc (x,y-1)) then [{start= (x,y); destination = (x, y-1); piece_captured =
  (check_position b (x, y+1))}] else []
 (*black piece, other side*)
 | false, true-> let raw_pos = [(x+1, y); (x-1, y); (x, y-1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match check_position b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) then [{start= (x,y); destination = p ;
 piece_captured = (check_position b p)}] else [] ) raw_pos)




let move_cannon (b:board) (pc:piece) ((x,y): position) :step list =
  begin
    let result = ref [] in
    let flag=ref false in
    let rec loop_forward curr_position=
    if !flag=false then
      begin
      match  (check_position b curr_position), (in_bound curr_position) with
       | None, true -> (result := {start = (x,y); destination = curr_position ;
        piece_captured = None}::(!result);
        loop_forward (fst curr_position, (snd curr_position)+1))
       | Some sth ,true -> (flag:=true;
       loop_forward (fst curr_position, (snd curr_position)+1))
       | _ , _ -> ()
      end
    else
      begin
      match  (check_position b curr_position), (in_bound curr_position) with
       | Some sth ,true ->

                  if sth.team=pc.team then ()

                  else

                  result := {start = (x,y); destination = curr_position;

                  piece_captured = Some sth}::(!result)

       | None, true -> loop_forward (fst curr_position, (snd curr_position)+1)

       | _ , _ -> ()

      end

    in

    let rec loop_back curr_position =
    if !flag=false then
      begin
      match  (check_position b curr_position), (in_bound curr_position) with

       | None, true -> result := {start = (x,y); destination = curr_position ;

        piece_captured = None}::(!result); loop_back (fst curr_position, (snd curr_position)-1)

       | Some sth ,true -> flag:=true; loop_back (fst curr_position, (snd curr_position)-1)

       | _ , _ -> ()

      end

    else

      begin

      match  (check_position b curr_position), (in_bound curr_position) with

       | Some sth ,true -> if sth.team=pc.team then ()

                           else

                  result := {start = (x,y); destination = curr_position;

                  piece_captured = Some sth}::(!result)

       | None, true -> loop_back (fst curr_position, (snd curr_position)-1)

       | _ , _ -> ()

      end

    in

    let rec loop_left curr_position =

    if !flag=false then

      begin

      match  (check_position b curr_position), (in_bound curr_position) with

       | None, true -> result := {start = (x,y); destination = curr_position ;

        piece_captured = None}::(!result); loop_left ((fst curr_position)-1, snd curr_position)

       | Some sth ,true -> flag:=true; loop_left  ((fst curr_position)-1, snd curr_position)

       | _ , _ -> ()

      end

    else

      begin

      match  (check_position b curr_position), (in_bound curr_position) with

       | Some sth ,true -> if sth.team=pc.team then ()

                           else

                  result := {start = (x,y); destination = curr_position;

                  piece_captured = Some sth}::(!result)

       | None, true -> loop_left  ((fst curr_position)-1, snd curr_position)

       | _ , _ -> ()

      end

    in

    let rec loop_right curr_position =

    if !flag=false then

      begin

      match  (check_position b curr_position), (in_bound curr_position) with

       | None, true -> result := {start = (x,y); destination = curr_position ;

        piece_captured = None}::(!result); loop_right ((fst curr_position)+1, snd curr_position)

       | Some sth ,true -> flag:=true; loop_right  ((fst curr_position)+1, snd curr_position)

       | _ , _ -> ()

      end

    else

      begin

      match  (check_position b curr_position), (in_bound curr_position) with

       | Some sth ,true -> if sth.team=pc.team then ()

                           else

                  result := {start = (x,y); destination = curr_position;

                  piece_captured = Some sth}::(!result)

       | None, true -> loop_right  ((fst curr_position)+1, snd curr_position)

       | _ , _ -> ()

      end

      in

      let ()=result:=[] in

    let ()=flag:=false in

    let ()=loop_forward (x, y+1) in

    let ()=flag:=false in

    let ()=loop_back (x, y-1) in

    let ()=flag:=false in

    let ()=loop_right (x+1 , y) in

    let ()=flag:=false in

    let ()=loop_left (x-1, y) in

    !result
  end


let move_horse (b:board) (pc:piece) ((x,y): position) :step list =
(*red piece in its own part: row 0-5*)
 let raw_hori_pos = [(x+2, y+1); (x+2, y-1); (x-2,y+1); (x-2,y-1)] in
 let hori_pos = List.flatten (List.map (fun p -> if (in_bound p) && (
    match check_position b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && ( check_position b ((fst p +x) /2 , y) = None) then [{start= (x,y);

 destination = p;

    piece_captured = (check_position b p )}] else []) raw_hori_pos)

in let raw_vert_pos = [(x+1, y+2) ; (x+1,y-2); (x-1, y+2); (x-1, y-2)] in
  let vert_pos = List.flatten (List.map (fun p -> if (in_bound p) && (
    match check_position b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && ( check_position b (x,((snd p)+ y)/2  ) = None) then [{start= (x,y);
destination = p;
    piece_captured = (check_position b p)}] else []) raw_vert_pos)
in hori_pos@vert_pos





let move_elephant (b:board) (pc:piece) ((x,y): position) :step list =

  let raw_pos=[(2,2); (-2,-2); (2,-2); (-2, 2)] in

  match (self_side pc (x,y)) with

 (*self side*)

 | true -> List.flatten (List.map (fun p ->

     if self_side pc (x+(fst p), y+(snd p)) &&

        check_position b (x+(fst p)/2, y+(snd p)/2)=None

     then

      match (check_position b (x+(fst p), y+(snd p))) with

      |None->  [{start= (x,y); destination = (x+(fst p), y+(snd p));

     piece_captured = None}]

      |Some sth->if sth.team<>pc.team then

       [{start= (x,y); destination = (x+(fst p), y+(snd p));

        piece_captured = (check_position b p)}]

                 else []

     else []) raw_pos)

 (*other side of river*)

 | false -> []





let move_advisor (b:board) (pc:piece) ((x,y): position) : step list =

let raw_pos = [(x+1, y+1); (x-1, y-1); (x-1, y+1); (x+1, y-1) ] in

 List.flatten (List.map (fun p -> if (in_bound p) && (

    match check_position b p with

    | Some sth-> sth.team <> pc.team

    | None -> true

 ) && (in_square pc p)then [{start= (x,y); destination = p;

    piece_captured = (check_position b p)}] else []) raw_pos )





let move_general (b:board) (pc:piece) ((x,y): position) : step list =

let raw_pos = [(x+1, y); (x-1, y); (x, y+1); (x, y-1) ] in

 List.flatten (List.map (fun p -> if (in_bound p) && (in_square pc p)&& (

    match check_position  b p with

    | Some sth-> sth.team <> pc.team

    | None -> true

 )  then [{start= (x,y); destination = p;

    piece_captured = (check_position b p)}] else []) raw_pos )



  (*check*)





let update_board (b:board)  (s:step) : unit  =

 (*let (xs,ys) = s.start in

 let (xe,ye) = s.destination in

 Printf.printf "starting point is %d, %d\n" xs ys;

 Printf.printf "destination point is %d, %d\n" xe ye;*)

 change_entry b s.start s.destination s.piece_captured



 (**)





let update_prev (s:step)  (pv:prev_step) :prev_step =

 begin

  (*match pv with

  | [] -> [s]

  | hd::tl -> begin match tl with

              | []->hd::[s]

              | h::[] -> hd :: h :: [s]

              | _->tl@[s]

              end*)

  s::pv

end



(*THIS IS WRONG!! b still gets mutated!*)

let update_unmutable (s:step) (b:board) (p:prev_step) =
  let p_next=update_prev s p in
  let b_copy = Board.copy b in
  let ()=update_board b_copy s in
  (b_copy,p_next)


let additional_rules_1 (b:board) (pv:prev_step) (s:step)=
  match check_position b s.start with
  | None->false
  | Some p->
             begin
             match p.type_of with
             | General-> if p.name="GB" then
                  begin
                  match (get_position b "GR") with
                  |None-> false
                  |Some opp_pos->
                      if (fst s.destination)=(fst opp_pos) then
                        let x=fst opp_pos in
                        let flag=ref false in
                        (for y = (snd opp_pos) to (snd s.destination) do
                          match check_position b (x,y) with
                          |None->()
                          |Some x->flag:=true
                        done);
                        (if !flag=true then true
                        else false)
                      else true
                  end
                 else
                  begin
                  match (get_position b "GB") with
                  |None-> false
                  |Some opp_pos->if (fst s.destination) =
                    (fst opp_pos)  then
                        let x=fst opp_pos in
                        let flag=ref false in
                        (for y = (snd s.destination) to (snd opp_pos) do
                          match check_position b (x,y) with
                          |None->()
                          |Some x->flag:=true
                        done);
                        (if !flag=true then true
                        else false)
                    else true
                  end
             | _ -> true
             end




let check_repeat (p:prev_step) (s:step) : bool =

  let l = List.length p in

  if l >= 8 then

    (s = (List.nth p 3)) && (s = (List.nth p 7)) && ((List.nth p 1) = (List.nth p 5))

  else false





let additional_rules (b:board) (pv:prev_step) (s:step)=

  (additional_rules_1 b pv s) && (not (check_repeat pv s))





let generate_piece_move (b:board) (pv:prev_step) (p:piece)=

  match (get_position b p.name) with

  |None-> raise InvalidMove

  |Some pos->begin let mvs=

    match p.type_of with

     | General -> move_general b p pos

     | Advisor -> move_advisor b p pos

     | Elephant -> move_elephant b p pos

     | Horse    -> move_horse b p pos

     | Rook     -> move_rook b p pos

     | Cannon   -> move_cannon b p pos

     | Soldier  -> move_soldier b p pos

  in let candidate=List.filter (fun m->additional_rules b pv m) mvs in

  if List.length candidate>0 then  candidate

  else [] end



let generate_all_moves (b:board) (p:prev_step) (side:bool): step list=

  let all_pieces = get_alive_side b side in

  let each_steps = List.map

    (fun a -> generate_piece_move b p a) all_pieces in

  List.flatten each_steps







let check_valid (b: board) (pv:prev_step) (st:step) :bool =

  let start = st.start in

  let pc = match (check_position b start ) with

    | None -> raise InvalidMove

    | Some piece -> piece

  in

  let psbl_list = generate_piece_move b pv pc  in

  let check_single s ls = (s.start=ls.start) && (s.destination = ls.destination)

  in List.exists (fun a -> check_single st a ) psbl_list





let check_win (b:board) (pv : prev_step) (st:step) :bool=

  match st.piece_captured with

  | None -> false

  | Some pc -> begin match pc.type_of with

    | General -> true

    | _ -> false

  end



let checked (b:board) (pv:prev_step) (cur_side:bool)=

  let all_moves=generate_all_moves b pv cur_side in

  List.exists (fun a->check_win b pv a) all_moves



(*In main, still need to ensure that the undos are restricted*)

(* let undo_one (b:board) (ps : prev_step) : prev_step =

  Printf.printf " Here is undo_one! \n THe length of prev_step is now: \n";

  print_int (List.length ps);

  let rect_step = List.hd ps in

  Printf.printf " \nLIst.hd is done \n";

  let dest_piece = check_position b rect_step.destination in

  let dest = rect_step.destination in

  let strt = rect_step.start in

  let () = (get_boardArray b).((fst dest) ).((snd dest) ) <- rect_step.piece_captured in

  let () = (get_boardArray b).((fst strt) ).((snd strt) ) <- dest_piece in

  List.tl ps (*need to also update hashtable*) *)



let undo_one (b:board) (ps : prev_step) : prev_step =

 match ps with

  | [] -> failwith "prev_step is empty."

  | hd::tl ->

    (

    let rect_step = hd in

    let dest_piece = check_position b rect_step.destination in

    let dest = rect_step.destination in

    let strt = rect_step.start in

    let () = (get_boardArray b).((snd dest)-1).((fst dest)-1) <- rect_step.piece_captured in

    let () = (get_boardArray b).((snd strt)-1).((fst strt)-1) <- dest_piece in

    tl

    )


let undo (b:board) (ps : prev_step ) : prev_step =

  let temp = undo_one b ps in undo_one b temp

(*

1.generals cannot face each other

2. interactions

*)
