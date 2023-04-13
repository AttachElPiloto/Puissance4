
(*************** Logique de la grille **************)

type joueur = J1 | J2
type etat = (joueur option) array array

let adversaire j = match j with
 | J1 -> J2
 | J2 -> J1

let init () =
  let grille = Array.make 7 [||] in
  for i = 0 to 6 do
    grille.(i) <- Array.make 5 None
  done;
  grille

let complet grille = 
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in 
  try 
  for i = 0 to lng - 1 do
    match grille.(i).(lrg-1) with
    | None -> raise Exit
    |_ -> ()
  done;
  true;
  with Exit -> false;;

complet (init ());;
let peut_jouer grille colonne =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if colonne >= lng then failwith "colonne invalide"
  else
    try
    for i=0 to lrg -1 do
      match grille.(colonne).(i) with
      |None -> raise Exit
      |_ -> ()
    done;false;
    with Exit -> true;;
peut_jouer (init()) 0;;
let jouer grille colonne j =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if (not (peut_jouer grille colonne) ) then failwith "coup impossible"
  else 
    try
    for i=0 to lrg - 1 do
      match grille.(colonne).(i) with
      |None -> grille.(colonne).(i) <- Some(j);raise Exit
      |_ -> ()
    done
    with Exit -> ()
  ;;
    

let red = "\x1b[41m"
let blue = "\x1b[44m"
let reset = "\x1b[0m"

let afficher grille =
  let ligne i = 
    Printf.printf "|";
    for j = 0 to 6 do
      match grille.(j).(i) with
        | None -> Printf.printf "   |"
        | Some J1 -> Printf.printf "%s X %s|" red reset
        | Some J2 -> Printf.printf "%s O %s|" blue reset
    done;
    Printf.printf "\n"
  in
  (* Indication des numéros de colonnes *)
  Printf.printf "\n";
  for i = 0 to 6 do
    Printf.printf "  %d " i
  done;
  (* Lignes *)
  Printf.printf "\n";
  for i = 4 downto 0 do
    ligne i
  done;
  (* Socle *)
  Printf.printf "+";
  for _ = 0 to 6 do Printf.printf "===+" done;
  Printf.printf "\n"
;;

 let test_jouer () =
  let grille = init () in 
  afficher grille;
  jouer grille 0 J1;
  afficher grille;
  jouer grille 0 J2;
  afficher grille;
  jouer grille 0 J1;
  afficher grille;
  jouer grille 0 J2;
  afficher grille;
  jouer grille 0 J1;
  afficher grille;
  jouer grille 0 J2;
  afficher grille
  ;;
(* test_jouer();; *)
let check_diag_r grille i j joueur =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if (j+3>=lrg || i+3>=lng) then false
  else 
    match grille.(i).(j),grille.(i+1).(j+1),grille.(i+2).(j+2),grille.(i+3).(j+3) with
    |Some(a),Some(b),Some(c),Some(d) when a=joueur && b=joueur && c=joueur && d=joueur
    -> true
    |_ -> false
;;

let check_diag_l grille i j joueur =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if (j+3>=lrg || i-3<0) then false
  else 
    match grille.(i).(j),grille.(i-1).(j+1),grille.(i-2).(j+2),grille.(i-3).(j+3) with
    |Some(a),Some(b),Some(c),Some(d) when a=joueur && b=joueur && c=joueur && d=joueur
    -> true
    |_ -> false
;;
  
  
let check_up grille i j joueur =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if (j+3>=lrg) then false
  else 
    match grille.(i).(j),grille.(i).(j+1),grille.(i).(j+2),grille.(i).(j+3) with
    |Some(a),Some(b),Some(c),Some(d) when a=joueur && b=joueur && c=joueur && d=joueur
    -> true
    |_ -> false
;;

let check_right grille i j joueur =
  let lng = Array.length grille in
  let lrg = Array.length grille.(0) in
  if (i+3>=lng) then false
  else 
    match grille.(i).(j),grille.(i+1).(j),grille.(i+2).(j),grille.(i+3).(j) with
    |Some(a),Some(b),Some(c),Some(d) when a=joueur && b=joueur && c=joueur && d=joueur
    -> true
    |_ -> false
;;
  
exception Victoire
let victoire e joueur =
  let lng = Array.length e in
  let lrg = Array.length e.(0) in
  try 
  for i=0 to lng -1 do
    for j=0 to lrg-1 do
    let a,b,c,d=check_diag_r e i j joueur,check_diag_l e i j joueur,check_up e i j joueur,check_right e i j joueur in 
    if a||b||c||d then 
    raise Victoire
    done
  done;false
 with Victoire -> true;;


let test_victoire1 ()=
 let grille = init() in 
 jouer grille 0 J2;
 jouer grille 0 J2;
 jouer grille 0 J2;
 jouer grille 1 J2;
 jouer grille 1 J2;
 jouer grille 2 J2;
 jouer grille 1 J1;
 jouer grille 2 J1;
 jouer grille 3 J1;
 jouer grille 0 J1;
 afficher grille;
 victoire grille J1;;
let test_victoire2 ()=
 let grille = init() in 
 jouer grille 0 J1;
 jouer grille 0 J1;
 jouer grille 0 J1;
 jouer grille 0 J1;
 afficher grille;
 victoire grille J1;;

(* test_victoire1 ();; *)
(* test_victoire2 ();; *)


(*************** Utilisateur **************)

let rec demander e =
  print_string "Colonne souhaitée : \n";
  let colonne = ref (read_int()) in
  if not (peut_jouer e !colonne) then 
    begin 
      while not (peut_jouer e !colonne ) do
      print_string "Colonne pas disponible, saisissez une nouvelle colonne\n";
      colonne :=  read_int()
      done
    end;
  jouer e !colonne J1;;

  

(*************** IA **************)

let successeur e j =
  let l = ref [] in 
  let lng = Array.length e in
  for i = 0 to lng-1 do 
    if peut_jouer e i then 
      (
        let c = Array.make lng [||] in
        for i=0 to lng-1 do
        c.(i)<- Array.copy e.(i)
        done;
        jouer c i j;l:=c::(!l))
  done ;
  !l;;

let test_sucesseur () =
  let grille = init () in 
  jouer grille 0 J1;
  jouer grille 0 J2;
  jouer grille 1 J1;
  let s=successeur grille J2 in 
  for i=0 to (List.length s - 1) do
    afficher (List.nth s i)
  done;;
(* test_sucesseur();; *)

let rec minmax e j d = 
  if victoire e J1 then -1000
  else if victoire e J2  then 1000
  else if d=0 then 0
  else 
    let l = successeur e j in
    let jn = ref J1 in 
    if j=J1 then jn:= J2;
    if List.length l = 0 then 0
    else
        let l' =List.map (fun x -> minmax x (!jn) (d-1)) l in
        let sum = ref (-1000) in
        if j=J2 then
        begin 
          for i = 0 to (List.length (l') -1) do
            if (List.nth (l') i = 1000 && !sum <1000 ) then sum:=1000;
            if (List.nth (l') i = 0 && !sum <0 ) then sum:=0;
          done;!sum;
        end
      else
        begin
          sum:=1000;
          for i = 0 to (List.length (l') -1) do
            if (List.nth (l') i = -1000 && !sum > -1000) then sum:=-1000;
            if (List.nth (l') i = 0 && !sum >0  ) then sum:=0;
          done;
          !sum
        end
     ;;      
let rec alphaBeta e j d alpha beta= 
  if victoire e J1 then -1000
  else if victoire e J2  then 1000
  else if d=0 then 0
  else 
    if j=J1 then 
      begin 
        let v = ref min_int in 
        let l = sucesseur e j in 
        for i=0 to List.length l -1 do
          v:= max v alphaBeta (List.nth i) J2 d-1 alpha beta
          if v>beta then v
          else alpha:=
          

    


let test_minmax()=
  let grille=init() in 
  jouer grille 0 J1;
  jouer grille 0 J2;
  jouer grille 1 J1;
  afficher grille;
  minmax grille J2 3;;

(* test_minmax ();; *)

let ind_max_tab tab =
let n = Array.length tab in
let ind = ref 0 in
let max = ref tab.(0) in
for i=0 to (n-1) do
    if tab.(i)> !max
    then ( ind := i;
        max := tab.(i))
    done;
!ind
;;
    
let rec ia (grille : etat) =
  let possibles = Array.make 7 (-10000) in
  for i=0 to 6 do
    let c = Array.make 7 [||] in
    for j=0 to 6 do
    c.(j)<- Array.copy grille.(j);
    done;
    if peut_jouer grille i
    then begin
          (jouer c i J2;
          possibles.(i)<-minmax c J1 4)
        end
  done;
  (* for i=0 to 6 do
    print_int possibles.(i);
    print_string " ";
  done; *)
  print_string "\n";
  jouer grille (ind_max_tab possibles) J2 ;;


(*************** Boucle de jeu principale **************)

let jouer() =
  let e = init () in
  afficher e;
  let j = ref J1 in
  while (not (complet e) && not (victoire e J1) && not (victoire e J2)) do
    (match !j with
    | J2 -> ia e
    | J1 -> demander e);
    afficher e;
    j := adversaire !j
  done;
  if (victoire e J1)
  then Printf.printf "Victoire J1\n"
  else Printf.printf "Victoire J2\n";;