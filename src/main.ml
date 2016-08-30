(*  The main file for REPL
 * including printing the chess board
 * printing the information needed for player
 * starting some dialogue with player
 * starting the game, ending the game, etc*)
open Board
open Move
open Piece
open Score
open AI
open Array
open Bytes

(*types for commands*)
type inputs = Undo| AI | TwoPlayer| Help | Forfeit | Restart| Exception| StartCo |
            EndCo | Other_input

let previous_Step = init_PrevStep()

(*Make them forfeit before they can quit*)

(* 1P vs AI is true and 2P is false
Red is true and black is falseteble *)
let curr_Board  = Board.init()
let init_Step = Move.init_step ()
type game_state =
{
(* AI  or 2 person  true=AI*)
game_mode: bool;
(* player choose color, red = true*)
color: bool;
(* who is playing, red = true *)
curr_color : bool ;
 (* start the new game *)
started: bool;
(* board carrying *)
board: board;
prev_step : prev_step;
curr_step : step;
undon: bool;
hard: bool
}

(* if red goes first *)
let init_GameState  =
  {game_mode = true;
  color = true;
  curr_color = true;
  started = false; board = curr_Board;
  prev_step = init_PrevStep() ;
  curr_step = init_step () ;
  undon = false;
  hard = false
  }

(* following funtion all have readline! *)
let input_Parse (input : bytes) : bytes list =
  let comma = Str.regexp "," in
  let input = Str.bounded_split comma (String.trim input) 2 in
    if List.length input > 1
      then [List.hd input] @ List.tl input
    else failwith "Incorrect input"

(*convert [x;y] to a position *)

let rec position_Convert (input: bytes list): int * int =
  if (List.length input = 2) then
    (int_of_string (List.hd input), int_of_string (List.hd (List.rev input)))
  else
    failwith "Incorrect coordinate size"

let valid_first_coor (gs  : game_state) (ps : position) : bool =

    match check_position gs.board ps with
    | None -> let () = print_endline "Nothing at this position" in false
    | Some x -> if (gs.curr_color = x.team) then true
                else
                let () = print_endline "This piece is not of your team's color!" in  false


let valid_snd_coor (gs  : game_state) (ps : position) : bool =
    match check_position gs.board ps  with
    | None -> true
    | Some x ->  
        if (gs.curr_color = x.team) then 
        (print_endline "This piece is of your team's color!"; false)
        else true

let print_piece (p : piece option) : unit =
  match  p with
  | None -> Printf.printf "%s%s" "\027[37m" "  -"
  (* red // green *)
  | Some pc -> let clr = if pc.team then "\027[31m " else "\027[32m " in
    Printf.printf "%s %s" clr pc.print_name
(*\xE2\x95\xB1 \xE2\x95\xB2  diagonal unicodes*)


let print_board (b: board) : unit =
  let line_counter = ref 0 in
  Printf.printf "%s%s" "\027[37m" "     1  2  3  4  5  6  7  8  9\n";
  Array.iter (fun inner_arr ->
  incr line_counter;
  if (!line_counter = 10) then Printf.printf "%s %d" "\027[37m" (!line_counter)
  else Printf.printf "%s %d" "\027[37m " (!line_counter) ;
  Array.iter print_piece inner_arr; Printf.printf "%s\n" "\027[37m")
  (get_boardArray b)

(* let run_board = print_board (init_board) *)

let rec init_game () : game_state =
  print_endline "\nWelcome to super cool command line Chinese Chess!";
  {init_GameState with board = Board.init()} |> choose_mode


and choose_mode (gs: game_state) : game_state =
  let () = print_endline "Type 'AI' to play with AI or type '2p' to play with another human" in
  let input = read_line () in
  match  lowercase (input) with
  | "ai" -> choose_difficulty {gs with game_mode = true}
  | "2p" -> choose_color {gs with game_mode = false}
  | "quit" -> run_quit gs
  | "restart" -> run_restart gs
  | _ -> print_endline "Input is not recognized. Please retype valid input."; choose_mode gs


(* let the user choose color, red always goes firs *)
and choose_color (gs:game_state) : game_state =
  let () = print_endline "Type 'red' to play first, type 'green' to play second.\nType 'restart' to restart.\nType 'quit' to quit game." in
  let input = read_line () in
  match lowercase (input) with
  | "red" -> run_round {gs with color = true}
  | "green" -> run_round {gs with color = false}
  | "quit" -> run_quit gs
  | "restart" -> run_restart gs
  | _ -> print_endline "Please type either 'red' or 'green'"; choose_color gs


and choose_difficulty (gs:game_state) : game_state =
  let () = print_endline
 "Type 'easy' to play with a 2110 (easy) AI. \nType 'hard' to play with a 3110 (hard) AI.\nType 'restart' to restart.\nType 'quit' to quit game." in
  let input = read_line () in
  match lowercase (input) with
  | "easy" -> choose_color {gs with hard =false}
  | "hard" -> choose_color {gs with color = true}
  | "quit" -> run_quit gs
  | "restart" -> run_restart gs
  | _ -> print_endline "Please type either 'easy' or 'hard'"; choose_difficulty gs


(* run a round *)
and run_round (gs:game_state) : game_state =
(* let () = print_board gs.board in *)
(* let () = print_endline "enter run_round" *)
  let () = print_endline ("This is "^(if gs.curr_color then "red" else "green")^" playing." )in

    if gs.game_mode && (not gs.color)=gs.curr_color then
    let () = print_endline "This is the AI's turn."
  in
  (if gs.hard then run_hard_ai gs else run_easy_ai gs)
  else
    let () = print_endline "This is the human's turn." in run_human gs

and run_undo (gs:game_state) : game_state =
  print_endline "Undo.";
  let new_pv = (* gs.prev_step *) undo gs.board gs.prev_step  in
  Printf.printf "undo_one in run_undo is done\n";
  run_round {gs with prev_step = new_pv }
and run_quit (gs:game_state) : game_state =
  print_endline "Byebye!";
  exit 0
  (*to the beginning of the game*)
and run_restart (gs:game_state) : game_state =
  print_endline "You restarted.";
  init_game ()
and run_back (gs:game_state) : game_state =
  let () = print_endline "Please retype starting point of form 'x,y'."
                 in first_coor gs



and first_coor (gs:game_state) : game_state =
   let () = print_board gs.board in
   let () = print_endline "Type the first piece you want to move, in the form 'x, y' .\nType 'undo' to undo one round.\nType 'restart' to restart.\nType 'quit' to quit game." in
   (* try(  *)let input = read_line () in
    match input with
    | "undo" ->  run_undo gs
    | "restart" -> run_restart gs
    | "quit" -> run_quit gs
    | sth ->  (let pos = input |> input_Parse |> position_Convert  in
              if ( pos |> (valid_first_coor gs )) then
              (print_endline ("You are moving the piece "^(piece_name (check_position gs.board pos)));
              let st = {gs.curr_step with start = pos} in
              second_coor {gs with curr_step = st})
              else (first_coor gs))
    (* with _ -> print_endline "Invalid input. Please input a valid coordinate";
    first_coor gs *)

(* operate on second coordinate *)
and second_coor (gs: game_state) : game_state =
   let () = print_endline "Type the destination you want to go to, in the form 'x, y'.\nType 'back' to retype your starting position.\nType 'restart' to restart game.\nType 'quit' to quit game." in
   try (let input = read_line () in
   match input with
    | "back" -> run_back gs
    | "quit" ->  run_quit gs
    | "restart" -> run_restart gs
    |  sth -> let pos = sth |> input_Parse |> position_Convert  in
   begin
   match pos |> valid_snd_coor gs with
   | true ->
      let st = {gs.curr_step with destination = pos;
    piece_captured = (pos |> check_position gs.board)} in
    if check_valid gs.board gs.prev_step st then begin

      let () = (print_endline ("You are moving to "^
        (string_of_position pos)^"and captured "
        ^(piece_name (pos |> check_position gs.board)))) in
      run_step {gs with curr_step = st } end else
      let () = print_endline "The inputted coordinate produces an invalid step. Please retype a valid coordinate"
      in second_coor gs
   | false -> print_endline "Invalid coordinate."; second_coor gs
 end )
 with _ -> print_endline "Invalid input. Retype a valid coordinate"; second_coor gs
(* update gs functions *)

and run_step (gs : game_state ) : game_state =

    match check_win gs.board gs.prev_step gs.curr_step with
      | true -> let () = print_endline
      ((if gs.curr_color then "RED" else "GREEN" )^" wins! " ) in init_game ()
      | false -> let () =  print_endline "Continue playing"  in
    let () = update_board gs.board gs.curr_step in
      run_round {gs with prev_step = gs.prev_step |>
      update_prev gs.curr_step ; curr_color= not (gs.curr_color)}


and run_human (gs: game_state ) : game_state =
  gs |> first_coor |>second_coor


and run_hard_ai (gs: game_state) : game_state =
  let () = print_board gs.board in
  let () = print_endline "The AI is thinking. Please wait patiently." in
  let bst_step =  hard_AI gs.board gs.prev_step (not gs.color) in
  let up_gs = {gs with curr_step = bst_step} in
  run_step up_gs

and run_easy_ai (gs: game_state) : game_state =
  let () = print_board gs.board in
  let () = print_endline "The AI is thinking. Please wait patiently." in
  let bst_step =  easy_AI gs.board gs.prev_step (not gs.color) in
  let up_gs = {gs with curr_step = bst_step} in
  run_step up_gs

let _ = init_game ()
