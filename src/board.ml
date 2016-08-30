(*open Core.Std*)
open Piece
exception InvalidMove

(*board={first=piece array array; second=(string,piece) Hashtbl}
 *)
type board={first:piece option array array; second:(string, position) Hashtbl.t}

(* check whether the position is on the board
 * duplicate with the function in move
 * handle later *)
let in_bound ((x,y):position) : bool=
 x>=1 && x<=9 && y>=1 && y <=10

(*get the piece given position*)
let check_position (b:board) (p:position) =
  if in_bound p then
  ((b.first).((snd p)-1)).((fst p)-1)
  else None

(*the following function can tell whether a piece still exists on the borad*)
let check_alive (b:board) (pie:string) = Hashtbl.mem (b.second) pie

(*get the position given a piece*)
let get_position (b:board) (pie:string) =
  if check_alive b pie = true then
  Some (Hashtbl.find (b.second) pie)
  else None

(*giving all the pieces which are still alive*)
let get_alive_pieces (b:board)=Hashtbl.fold
(fun s p lst->let p=(b.first).((snd p)-1).((fst p)-1) in
               match p with
               | None->lst
               | Some x->x::lst) (b.second) []


let get_alive_side (b:board) (side:bool) =
  let alive_piece=get_alive_pieces b in
  List.filter (fun p->p.team=side) alive_piece

(* p1 is the start position
 * p2 is the destination position
 * pc is the piece captured
*)
let change_entry (b:board) (p1:position) (p2:position) (pcapture:piece option)=
  let pc=check_position b p1 in
  let (odx, ody) = p1 in
  let (nwx, nwy) = p2 in

  let ()= match pc with
   | None -> Printf.printf "Starting position none\n"
   | Some p->
      b.first.(ody-1).(odx-1) <- None;
      b.first.(nwy-1).(nwx-1) <- pc;
      Hashtbl.replace b.second p.name p2
   in
   match pcapture with
   | None -> ()
   | Some p -> Hashtbl.remove b.second p.name

let change_one (b:board) (p:position) (pnew:piece option) : unit=
  let (x, y) = p in
  b.first.(y-1).(x-1)<- pnew


let copy (b:board)=
  let f=b.first in
  let s=b.second in
  let f_tempt=Array.fold_left (fun lst row->lst@[Array.copy row]) [] f in
  let f_copy=Array.of_list f_tempt in
  {first=f_copy;second=Hashtbl.copy s}



let init ()=
  let r1=Array.of_list [Some rookR1;Some horseR1; Some elepR1;
  Some advisorR1; Some generalR; Some advisorR2;Some elepR2;
  Some horseR2; Some rookR2] in
  let r2=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r3=Array.of_list [None; Some canR1; None; None; None; None; None; Some canR2; None]
  in
  let r4=Array.of_list [Some soldR1; None; Some soldR2; None;
  Some soldR3; None; Some soldR4; None;
  Some soldR5] in
  let r5=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r6=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r7=Array.of_list [Some soldB1; None; Some soldB2; None;
  Some soldB3; None; Some soldB4; None;
  Some soldB5] in
  let r8=Array.of_list [None; Some canB1; None; None; None; None; None; Some canB2; None]
  in
  let r9=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r10=Array.of_list [Some rookB1;Some horseB1; Some elepB1;
  Some advisorB1; Some generalB; Some advisorB2;Some elepB2;
  Some horseB2; Some rookB2] in
  let f1= Array.of_list [r1;r2;r3;r4;r5;r6;r7;r8;r9;r10] in
  let f2= Hashtbl.create 32 in
  let ()=Hashtbl.add f2 "GR" (5,1);Hashtbl.add f2 "GB" (5, 10);
  Hashtbl.add f2 "A1R" (4,1);Hashtbl.add f2 "A2R" (6,1);
  Hashtbl.add f2 "A1B" (4,10);Hashtbl.add f2 "A2B" (6,10);
  Hashtbl.add f2 "ER1" (3,1); Hashtbl.add f2 "ER2" (7,1);
  Hashtbl.add f2 "EB1" (3,10);Hashtbl.add f2 "EB2" (7,10);
  Hashtbl.add f2 "HR1" (2,1);Hashtbl.add f2 "HR2" (8,1);
  Hashtbl.add f2 "HB1" (2,10);Hashtbl.add f2 "HB2" (8,10);
  Hashtbl.add f2 "RR1" (1,1);Hashtbl.add f2 "RR2" (9,1);
  Hashtbl.add f2 "RB1" (1,10);Hashtbl.add f2 "RB2" (9,10);
  Hashtbl.add f2 "CR1" (2,3); Hashtbl.add f2 "CR2" (8,3);
  Hashtbl.add f2 "CB1" (2,8);Hashtbl.add f2 "CB2" (8,8);
  Hashtbl.add f2 "SR1" (1,4);Hashtbl.add f2 "SR2" (3,4);
  Hashtbl.add f2 "SR3" (5,4);Hashtbl.add f2 "SR4" (7,4);
  Hashtbl.add f2 "SR5" (9,4);Hashtbl.add f2 "SB1" (1,7);
  Hashtbl.add f2 "SB2" (3,7);Hashtbl.add f2 "SB3" (5,7);
  Hashtbl.add f2 "SB4" (7,7); Hashtbl.add f2 "SB5" (9,7);
  in
  {first=f1;second=f2}

let get_boardArray (b: board) =
  b.first

(*print the peice*)
let print_piece (p:piece option)=
  match  p with
  | None -> Printf.printf "%s%s" "\027[37m" "  -"
  (* red // green *)
  | Some pc -> let clr = if pc.team then "\027[31m " else "\027[32m " in
    Printf.printf "%s %s" clr pc.print_name

(*print the board*)
let print_board (b:board)=
  let line_counter = ref 0 in
  Printf.printf "%s%s" "\027[37m" "     1  2  3  4  5  6  7  8  9\n";
  Array.iter (fun inner_arr ->
  incr line_counter;
  if (!line_counter = 10) then Printf.printf "%s %d" "\027[37m" (!line_counter)
  else Printf.printf "%s %d" "\027[37m " (!line_counter) ;
  Array.iter print_piece inner_arr; Printf.printf "%s\n" "\027[37m")
  (get_boardArray b)


