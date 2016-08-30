type piece_type = General|Advisor|Elephant|Horse|Rook|Cannon|Soldier
type position = int*int

type piece={
name:string;
print_name : string;
(* color*)
type_of: piece_type;
team: bool;
}

(*general*)
let generalR= {name= "GR"; print_name= "\xE2\x99\x94";
type_of=General;team = true }

let generalB= {name= "GB"; print_name= "\xE2\x99\x94";
 type_of=General;team = false
}

let advisorR1= {name = "A1R"; print_name= "\xE2\x99\x97";
type_of = Advisor; team = true }

let advisorR2= {name = "A2R"; print_name= "\xE2\x99\x97";
type_of = Advisor; team = true }

let advisorB1= {name = "A1B"; print_name= "\xE2\x99\x97";
type_of = Advisor; team =false }

let advisorB2= {name = "A2B"; print_name= "\xE2\x99\x97";
type_of = Advisor; team =false  }

let elepR1= {name = "ER1"; print_name = "\xE2\x9A\x97"; type_of=
Elephant; team = true}
let elepR2= {name = "ER2"; print_name = "\xE2\x9A\x97"; type_of=
Elephant; team = true}
let elepB1= {name = "EB1"; print_name = "\xE2\x9A\x97"; type_of=
Elephant; team =false }
let elepB2= {name = "EB2"; print_name = "\xE2\x9A\x97"; type_of=
Elephant; team =false}


let horseR1 = {name= "HR1"; print_name = "\xE2\x99\x98";  type_of =
  Horse ; team = true }
let horseR2 = {name= "HR2"; print_name = "\xE2\x99\x98";  type_of =
  Horse ; team = true }
let horseB1 = {name= "HB1"; print_name = "\xE2\x99\x98";  type_of =
  Horse ; team = false}
let horseB2 = {name= "HB2"; print_name = "\xE2\x99\x98"; type_of =
  Horse ; team = false}


let rookR1 = {name = "RR1"; print_name= "\xE2\x99\x96";
type_of = Rook; team =true}
let rookR2 = {name = "RR2"; print_name= "\xE2\x99\x96";
type_of = Rook; team = true}
let rookB1 = {name = "RB1"; print_name= "\xE2\x99\x96";
type_of = Rook; team = false}
let rookB2 = {name = "RB2"; print_name= "\xE2\x99\x96";
type_of = Rook; team = false}

let canR1 = {name= "CR1"; print_name = "\xE2\x98\xA2";
type_of = Cannon; team = true}
let canR2 = {name= "CR2"; print_name = "\xE2\x98\xA2";
type_of = Cannon; team = true}
let canB1 = {name= "CB1"; print_name = "\xE2\x98\xA2";
type_of = Cannon; team = false}
let canB2 = {name= "CB2"; print_name = "\xE2\x98\xA2";
type_of = Cannon; team = false}

let soldR1 = {name = "SR1"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = true }
let soldR2 = {name = "SR2"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = true }
let soldR3 = {name = "SR3"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = true }
let soldR4 = {name = "SR4"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = true }
let soldR5 = {name = "SR5"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = true }
let soldB1 = {name = "SB1"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = false }
let soldB2 = {name = "SB2"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = false }
let soldB3 = {name = "SB3"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = false }
let soldB4 = {name = "SB4"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = false }
let soldB5 = {name = "SB5"; print_name = "\xE2\x99\x99";
type_of = Soldier; team = false }

let string_of_position (x,y) =
  "( "^(string_of_int x)^", "^(string_of_int y)^")"

let string_of_piece pc =
  match pc with
  | None -> "Nothing"
  | Some p -> p.name
(* let print_position (ps:position ) = match ps with
  | x, y -> print_int x; print_int y *)
(*  | _ -> failwith "invalid"

let print_piece pc = print_bytes (pc.name^"a "); print_position pc.position
*)

let piece_name (input: piece option): bytes=

begin
  match input with
  |None -> "None"
  | Some pc -> begin match pc.type_of with

    | General   -> "General"
    | Advisor   -> "Advisor"
    | Elephant  -> "Elephant"
    | Horse     -> "Horse"
    | Rook      -> "Chariot"
    | Cannon    -> "Cannon"
    | Soldier   -> "Soldier"
  end
end
