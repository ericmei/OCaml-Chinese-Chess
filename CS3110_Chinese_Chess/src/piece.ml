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
let generalR= {name= "GR"; print_name="G";
type_of=General;team = true }

let generalB= {name= "GB"; print_name="G";
 type_of=General;team = false
}

let advisorR1= {name = "A1R"; print_name= "A";
type_of = Advisor; team = true }

let advisorR2= {name = "A2R"; print_name= "A";
type_of = Advisor; team = true }

let advisorB1= {name = "A1B"; print_name= "A";
type_of = Advisor; team =false }

let advisorB2= {name = "A2B"; print_name= "A";
type_of = Advisor; team =false  }

let elepR1= {name = "ER1"; print_name = "E"; type_of=
Elephant; team = true}
let elepR2= {name = "ER2"; print_name = "E"; type_of=
Elephant; team = true}
let elepB1= {name = "EB1"; print_name = "E"; type_of=
Elephant; team =false }
let elepB2= {name = "EB2"; print_name = "E"; type_of=
Elephant; team =false}


let horseR1 = {name= "HR1"; print_name = "H";  type_of =
  Horse ; team = true }
let horseR2 = {name= "HR2"; print_name = "H";  type_of =
  Horse ; team = true }
let horseB1 = {name= "HB1"; print_name = "H";  type_of =
  Horse ; team = false}
let horseB2 = {name= "HB2"; print_name = "H";  type_of =
  Horse ; team = false}


let rookR1 = {name = "RR1"; print_name= "R";
type_of = Rook; team =true}
let rookR2 = {name = "RR2"; print_name="R";
type_of = Rook; team = true}
let rookB1 = {name = "RB1"; print_name= "R";
type_of = Rook; team = true}
let rookB2 = {name = "RB2"; print_name= "R";
type_of = Rook; team = true}

let canR1 = {name= "CR1"; print_name = "C";
type_of = Rook; team = true}
let canR2 = {name= "CR2"; print_name = "C";
type_of = Rook; team = true}
let canB1 = {name= "CB1"; print_name = "C";
type_of = Rook; team = false}
let canB2 = {name= "CB2"; print_name = "C";
type_of = Rook; team = false}

let soldR1 = {name = "SR1"; print_name = "S";
type_of = Soldier; team = true }
let soldR2 = {name = "SR2"; print_name = "S";
type_of = Soldier; team = true }
let soldR3 = {name = "SR3"; print_name = "S";
type_of = Soldier; team = true }
let soldR4 = {name = "SR4"; print_name = "S";
type_of = Soldier; team = true }
let soldR5 = {name = "SR5"; print_name = "S";
type_of = Soldier; team = true }
let soldB1 = {name = "SB1"; print_name = "S";
type_of = Soldier; team = false }
let soldB2 = {name = "SB2"; print_name = "S";
type_of = Soldier; team = false }
let soldB3 = {name = "SB3"; print_name = "S";
type_of = Soldier; team = false }
let soldB4 = {name = "SB4"; print_name = "S";
type_of = Soldier; team = false }
let soldB5 = {name = "SB5"; print_name = "S";
type_of = Soldier; team = false }

let print_position (ps:position ) = match  ps with
  | x, y -> print_int x; print_int y
(*  | _ -> failwith "invalid"

let print_piece pc = print_bytes (pc.name^"at "); print_position pc.position
*)
