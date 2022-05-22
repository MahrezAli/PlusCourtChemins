(***********************************************************************
 *  Outils logiques et alorithmiques,
 *  Thibaut Balabonski @ Université Paris-Saclay
 *  L2 Info, printemps 2022
 *
 *  Éléments de code pour le DM.
 *  
 *  Note : ces Éléments sont regroupés dans un seul fichier, mais il
 *  peut-être judicieux de les séparer en plusieurs modules. 
 *  Par exemple : avoir un fichier pour le programme principal et des 
 *  modules à part pour  les quadtrees, les graphes, et chaque autre 
 *  structure de données.
 * 
 *  Certaines fonctions ne sont pas implémentées, et servent juste à 
 *  proposer une ossature de programme complet. N'hésitez pas à 
 *  modifier la signature de ces fonctions pour personnaliser la
 *  structure de votre projet.
 **********************************************************************)
(* Le DM a été éffectué sur Visual Code *)
(*  
 VERSION SIMPLISTE:
 QUESTION 1:
 Le terrain est de côté n, donc le tableau t est de taille n*n. La mémoire vive de mon ordinateur est de 8Go, il faut donc determiner à partir de quel n, n*n*8 > 8*10^9.
 supposons s = 2^30, alors 8*s = 8*2^30.
 donc lorseque n = 8*2^30, il est possible que l'odinateur soit saturé.
*)  
(**
   Type de données pour représenter un quadtree dont les régions libres
   sont numérotées. Pendant la construction, affectez arbitrairement le
   numéro (-1) à toutes les régions. La fonction  (numerote)  fournie 
   donnera un numéro unique à chaque région une fois l'arbre complet. 
*)
type qtree =
  | Libre of int (* numéro *)
  | Mur
  | Quad of qtree * qtree * qtree * qtree (* no, ne, so, se *)

(**
   Fonction de numérotation des quadtrees
     numerote: qtree -> int -> qtree * int

   L'appel  (numerote qt k)  renvoie une paire  (qt', k')  où 
   - (qt') est un quadtree de même structure que (qt) mais dont les 
     régions libres sont numérotées consécutivement à partir de (k)
   - (k') est l'entier qui suit le dernier numéro utilisé
 *)
let rec numerote qt k = match qt with
  | Libre _ -> Libre k, k+1
  | Mur     -> Mur, k
  | Quad(no, ne, so, se) ->
     let no, k = numerote no k in
     let ne, k = numerote ne k in
     let so, k = numerote so k in
     let se, k = numerote se k in
     Quad(no, ne, so, se), k
;;
(**
   Affichage d'un quadtree 
   (vue hiérarchique avec retrait proportionnel à la profondeur)
 *)     
open Printf
let print_qtree qt =
  let offset n = String.make n ' ' in
  let rec print o = function
    | Mur -> printf "%sMur\n" (offset o)
    | Libre k -> printf "%sLibre %d\n" (offset o) k
    | Quad(no, ne, so, se) ->
       printf "%sQuad\n" (offset o);
       print (o+2) no;
       print (o+2) ne;
       print (o+2) so;
       print (o+2) se
  in
  print 0 qt
;;

(**
   Type pour représenter un graphe pondéré dans la partie 2 : 
   tableau de listes d'adjacence

   Sous-entendu : les sommets d'un graphe sont numérotés consécutivement à
   partir de 0. Un graphe (g) et un numéro de sommet (i) étant donnés, g.(i)
   est une liste de paires, où chaque paire (v, d) contient
   - le numéro (v) d'un voisin
   - la distance (d) à ce voisin

   Deux fonctions d'affichage fournies
   (print_graph) donne une vue complète, longueurs des arètes comprises
   (print_graph_compact) 
 *)
type graph = (int * float) list array

let print_graph g =
  let n = Array.length g in
  printf "Graphe à %d sommets :\n" n;
  for i = 0 to n-1 do
    printf "Sommet %d:\n" (i+1);
    List.iter (fun (v, d) -> printf "  voisin %d à distance %f\n" v d) g.(i);
  done
;;
let print_graph_compact g =
  for i = 0 to Array.length g - 1 do
    printf "%d:" i;
    List.iter (fun (v, _) -> printf " %d" v) g.(i);
    printf "\n"
  done
;; 
  
(**
   Fonction de lecture du fichier d'entrée
     load: string -> (int * int) list * int

   L'appel  (load f)  lit le fichier dont le nom est donné par la 
   chaîne (f) et renvoie une paire  (murs, n)  où
   - (murs) est la liste des quadruplets (x, y, dx, dy) donnant les
     dimensions des r zones intraversables
   - (n) est la longueur du côté du terrain
   On suppose que le fichier (f) contient une description de terrain
   valide. Le résultat de (load) n'est pas spécifié sinon.
 *)
open Scanf
let load file =
  let c = Scanning.open_in file in
  let n = bscanf c "%d\n" (fun n -> n) in
  let r = bscanf c "%d\n" (fun r -> r) in
  let murs = ref [] in
  for _ = 1 to r do
    bscanf c "%d %d %d %d\n" (fun x y dx dy -> murs := (x, y, dx, dy) :: !murs)
  done;
  !murs, n
;;

(*Fonctions utiles :*)
(*Affiche une liste de paire de int*)
let rec print_tuples =
  function
  | [] -> ()
  | (a, b, c) :: rest ->
    Printf.printf "(%d, %d, %d)" a b c;
    Printf.printf "\n" ;
    print_tuples rest
;;
(*Affiche une liste de int*)
let rec print_solo =
  function
  | [] -> ()
  | a :: rest ->
    Printf.printf "%d " a;
    print_solo rest
;;
(*Affiche une liste de pair de int et liste de int*)
let rec print_triples =
  function
  | [] -> ()
  | (a, lst) :: rest ->
    Printf.printf "%d: " a ;
    print_solo lst;
    Printf.printf "\n" ;
    print_triples rest
;;
(*Affiche une liste de pair de float (fonction spécifique au Quadtree)*)
let rec print_tuples_float =
  function
  | [] -> ()
  | (n, (a, b)) :: rest ->
    Printf.printf "(%.2f, %.2f)" a b;
    Printf.printf "\n" ;
    print_tuples_float rest
;;
(*Affiche une liste de pair de float*)
let rec print_tuples_float2 =
  function
  | [] -> ()
  | (a, b) :: rest ->
    Printf.printf "(%.2f, %.2f)" a b;
    Printf.printf "\n" ;
    print_tuples_float2 rest
;;
(*Enlever les doublons d'une liste*)
let rem_from_left lst =
  let rec is_member n mlst =
    match mlst with
    | [] -> false
    | h::tl ->
        begin
          if h=n then true
          else is_member n tl
        end
  in
  let rec loop lbuf rbuf =
    match rbuf with
    | [] -> lbuf
    | h::tl ->
        begin
          if is_member h lbuf then loop lbuf tl
          else loop (h::lbuf) rbuf
        end
  in
  List.rev (loop [] lst)
;;
(*Calculer une puissance *)
let rec power2_float x =
  x*.x
;;
(*Supprime le premier element d'une pile :*)
let remove p =
  match !p with
    [] -> failwith "Empty list"
    |hd :: tl -> p := tl ;
  hd
;;
(*Ajoute un élement dans une pile :*)
let add p x =
  p := x :: !p
;;

(*QUESTION 2*)
(*Création du labyrinthe*)
let creeLaby lst n =
    let rec tableau tab lst = 
        match lst with
            [] -> tab
            |(x,y,dx,dy)::ll -> for i = x to dx + x -1 do
                                    for j = y to dy + y -1 do
                                        tab.(i).(j) <- 0
                                    done;
                                done;
                                tableau tab ll;
    in
    tableau (Array.make_matrix n n 1) lst    
;; 

(*QUESTION 3*)
(*Fonction qui donne un voisin accessible à partir d'une case dans un tableau*)
let voisin tab cc =
    if (fst cc)-1 >= 0 && tab.((fst cc)-1).(snd cc) == 1 then
        ((fst cc)-1, snd cc)
    else if (fst cc)+1 < (Array.length tab) && tab.((fst cc)+1).(snd cc) == 1 then
        ((fst cc)+1, snd cc)
    else if (snd cc)+1 < (Array.length tab) && tab.(fst cc).((snd cc)+1) == 1 then
        (fst cc, (snd cc)+1)
    else if (snd cc)-1 >= 0 && tab.(fst cc).((snd cc)-1) == 1 then
      (fst cc, (snd cc)-1)
    else
      failwith "fail"
;;
(*Exploration du labyrinthe*)
let parcours tabs departs arrivees n =
  let depart =
    if (fst(departs)) == n && (snd(departs)) <> n then
      ((n-1), (snd(departs)))
    else if (fst(departs)) <> n && (snd(departs)) == n then
      ((fst(departs)), (n-1))
    else if (fst(departs)) == n && (snd(departs)) == n then
      ((n-1), (n-1))
    else
      ((fst(departs)), (snd(departs))) 
  in 
  let arrivee =
    if (fst(arrivees)) == n && (snd(arrivees)) <> n then
      ((n-1), (snd(arrivees)))
    else if (fst(arrivees)) <> n && (snd(arrivees)) == n then
      ((fst(arrivees)), (n-1))
    else if (fst(arrivees)) == n && (snd(arrivees)) == n then
      ((n-1), (n-1))
    else
      ((fst(arrivees)), (snd(arrivees))) 
  in 
  let pos = ref depart in
  let pile = ref [ ] in
  let tab = tabs in
  
  tab.(fst !pos).(snd !pos) <- -1;
  while !pos <> arrivee do
    if ((fst !pos)-1 >= 0 && tab.((fst !pos)-1).(snd !pos) == 1) || 
       ((fst !pos)+1 < (Array.length tab) && tab.((fst !pos)+1).(snd !pos) == 1)||
       ((snd !pos)-1 >= 0 && tab.(fst !pos).((snd !pos)-1) == 1)||
       ((snd !pos)+1 < (Array.length tab) && tab.(fst !pos).((snd !pos)+1) == 1)  then begin
      add pile !pos ;
      pos := voisin tab !pos ;
      tab.(fst !pos).(snd !pos) <- -1;
    end
    else
      pos := remove pile
  done ;
  add pile arrivee;
  let res = arrivee :: !pile in
  res
;;
 
(*QUESTION 4*)
(*Fonction qui trace le chemin du point de départ au point d'arrivé *)
let trace_itin tab lst departs arrivees n =
  let depart =
    if (fst(departs)) == n && (snd(departs)) <> n then
      ((n-1), (snd(departs)))
    else if (fst(departs)) <> n && (snd(departs)) == n then
      ((fst(departs)), (n-1))
    else if (fst(departs)) == n && (snd(departs)) == n then
      ((n-1), (n-1))
    else
      ((fst(departs)), (snd(departs))) 
  in 
  let arrivee =
    if (fst(arrivees)) == n && (snd(arrivees)) <> n then
      ((n-1), (snd(arrivees)))
    else if (fst(arrivees)) <> n && (snd(arrivees)) == n then
      ((fst(arrivees)), (n-1))
    else if (fst(arrivees)) == n && (snd(arrivees)) == n then
      ((n-1), (n-1))
    else
      ((fst(arrivees)), (snd(arrivees))) 
  in 
  let rec trace_itin_aux tab lst =
    match lst with
      [] -> tab
      |(a,b)::l ->  if a == (fst arrivee) && b == (snd arrivee) then begin tab.(a).(b) <- 7; end
      else tab.(a).(b) <- 3; trace_itin_aux tab l
  in
  let t = trace_itin_aux tab (parcours tab depart arrivee n) in
  for i = 0 to (Array.length t)-1 do
    for j = 0 to (Array.length t)-1 do
      if t.(i).(j) == -1 then
        t.(i).(j) <- 1
    done;
  done;
  t
;;

(* VERSION EFFICACE :*)
(*Questions préliminaires*)

(*Question 1*)
(*Affichage du labyrinthe : 0 pour les murs et 1 pour les routes traversables*)
let rec print_tab_int tab n =
  Printf.printf "\n";
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            let case = tab.(j).((n-1)-i) in
            if case < 10 then
              Printf.printf "   %d" case
            else
              Printf.printf "  %d" case
        done;
    Printf.printf "\n";
    Printf.printf "\n"
    done;
;;

(*Question 2*)
(*
Les deux configurations possibles :
- Générant un quadTree le plus simple possible : Dans ce cas, chaque rectangle intraversable sera situé dans une région differentes d'un même Quad, un dans NE, NO, SE et SO. Ce qui est similaire à Quad(Mur,Libre,Libre,Lire) == Quad(Quad(Mur,Mur,Mur,Mur), Libre, Libre, Libre).
- Générant un quadTree le plus complexe possible : Chaque rectangle sera placé au extremité du grand quadTree : Quad(Quad(Mur,Libre,Libre,Libre),Quad(Libre,Mur,Libre,Libre),Quad(Libre,Libre,Mur,Libre),Quad(Libre,Libre,Libre,Mur))
 *)

(*Partie A*)

(*Question 1*)
(*Simplifier le quadtree*)
let rec simplifie qt =
    match qt with
        Libre(_) -> Libre(-1)
        |Mur -> Mur
        |Quad(Libre(_),Libre(_),Libre(_),Libre(_)) -> Libre(-1)
        |Quad(Mur,Mur,Mur,Mur) -> Mur
        |Quad(a,b,c,d) -> Quad((simplifie a), (simplifie b), (simplifie c), (simplifie d))
;;
(*Faire l'union de deux quadtrees*)
let rec union qt1 qt2 =
    match qt1, qt2 with
        Quad(a,b,c,d), Mur -> Mur
        |Mur, Mur -> Mur
        |Mur, Quad(a,b,c,d) -> Mur
        |Quad(a,b,c,d), Quad(aa,bb,cc,dd) -> Quad((union a aa), (union b bb), (union c cc), (union d dd))
        |Libre(_), Libre(_) -> Libre(-1)
        |Libre(_), Quad(a,b,c,d) -> Quad(a,b,c,d)
        |Quad(a,b,c,d), Libre(_) -> Quad(a,b,c,d)
        |_ -> Mur
;;
(*Fonction pour lister toutes les coordonnées des murs à partir d'une coordonnée et de la taille du mur*)
let lstCoordMur x y dx dy n =
    let res = ref [ ] in
    for i = x to x+dx-1 do
        for j = y to y+dy-1 do
            add res (i, j)
        done;
    done;
    !res
;;



(*Crée un quadtree avec 1 rectangle en paramètre qui représente le mur de taille 1*)
let mur2tree x y n =
    let rec mur2qtree_aux x y n =
        if x < n/2 && y < n/2 then begin
            if n == 1 && n == 1 then
                    Mur
            else
                Quad(Libre(-1), Libre(-1), (mur2qtree_aux x y (n/2)), Libre(-1))
        end
        else if x < n/2 && y >= n/2  then begin
            if n == 1 && n == 1 then
                Mur
            else
                Quad((mur2qtree_aux x (y-(n/2)) (n/2)), Libre(-1), Libre(-1), Libre(-1))
        end
        else if x >= n/2 && y < n/2  then begin
            if n == 1 && n == 1 then
                Mur
            else
                Quad(Libre(-1), Libre(-1), Libre(-1), (mur2qtree_aux (x-(n/2)) y (n/2)))
        end
        else if x >= n/2 && y >= n/2  then begin
            if n == 1 && n == 1 then
                Mur
            else
                Quad(Libre(-1), (mur2qtree_aux (x-(n/2)) (y-(n/2)) (n/2)), Libre(-1), Libre(-1))
        end
        else
            failwith "Coordonnee invalide !"
    in
    mur2qtree_aux x y n
;;
(*Fonction qui va faire l'union de tous les quadtrees avec mur de taille 1 ce qui va permettre de créer un mur de taille (dx,dy)*)
let quadTree lst n =
    let res = ref (Libre(-1)) in
    let rec qt_aux lst =
        match lst with
            [] -> res
            |(x,y)::l -> res := simplifie (union !res (mur2tree x y n));
                        qt_aux l;
    in
    simplifie(!(qt_aux lst)) 
;;

(*Question 2*)
let rec intersection qt1 qt2 =
    match qt1, qt2 with
        Quad(a,b,c,d), Mur -> Quad(a,b,c,d)
        |Mur, Mur -> Mur
        |Mur, Quad(a,b,c,d) -> Quad(a,b,c,d)
        |Quad(a,b,c,d), Quad(aa,bb,cc,dd) -> Quad((intersection a aa), (intersection b bb), (intersection c cc), (intersection d dd))
        |_ -> Libre(-1)
;;

(*Question 3*)
(**
   Fonction de construction d'un quadtree à partir d'une liste de
   régions intraversables
     list2qtree: int -> (int * int * int * int) list -> qtree

   Arguments : 
   - (n) est la longueur du côté du terrain
   - (l) est la liste des rectangles intraversables
 *)
let rec list2qtrees n l =
  let qt = ref [] in
  let rec list2qtrees_aux n l =
    match l with
      [] -> qt
      |(x,y,dx,dy)::ll -> let list1 = lstCoordMur x y dx dy n in
                        add qt (quadTree list1 n);
                        list2qtrees_aux n ll;
  in
  qt := !(list2qtrees_aux n l);
  let rec jmaa lst c =
    match lst with
      [] -> Libre(-1)(*(List.nth (!qt) 0) *)
      |[e] -> if c > 0 then e else Libre(-1)
      |e1::e2::l -> jmaa ((union e1 e2)::l) (c+1)
  in 
  jmaa (!qt) 0
;;

(*Partie B*)
(*Question 1*)
let quadTreeNumerote lst n =
    let res = ref (Libre(-1)) in
    let rec qt_aux lst =
        match lst with
            [] -> res
            |(x,y)::l -> res := simplifie (union !res (mur2tree x y n));
                        qt_aux l;
    in
    let qtSansNum =  (simplifie( !(qt_aux lst))) in
    let qtSansNum2 = (simplifie(qtSansNum)) in
    let qtAvecNum = (numerote qtSansNum2 1) in
    qtAvecNum
;;

(*Question 2*)
let addCoord cc1 cc2 =
  match cc1, cc2 with
    (a,b),(c,d) -> (a+.c, b+.d)
;;
let addCoordInt cc1 cc2 =
  match cc1, cc2 with
    (a,b),(c,d) -> (a+c, b+d)
;;
let tabCoordCenter qt t =
    let lst = ref [] in
    let n = Float.of_int t in
    let qqt = fst(qt) in
    let rec tabCoordCenter_aux qqt n cd = 
        match qqt with
            Mur -> ()
            |Libre(i) -> (let coordCenter = ((Float.div n 2.0), (Float.div n 2.0)) in
                        let coordPoint = 
                          let cc = cd in
                        addCoord coordCenter cc in
                        add lst (Libre(i), coordPoint))
            |Quad(a,b,c,d) -> tabCoordCenter_aux a (Float.div n 2.0) (addCoord cd (0.0,(n/.2.0))); tabCoordCenter_aux b (Float.div n 2.0) (addCoord cd ((n/.2.0),(n/.2.0))); tabCoordCenter_aux c (Float.div n 2.0) (addCoord cd (0.0,0.0)); tabCoordCenter_aux d (Float.div n 2.0) (addCoord cd ((n/.2.0),0.0)); 
    in
    tabCoordCenter_aux qqt n (0.0,0.0);
    !lst
;;

let rec printTest lst =
  let trouvenum qt =
    match qt with
      Libre(i) -> i 
      |_ -> 0
  in
  match lst with 
    [] -> ()
    |(a, b) :: rest ->
    Printf.printf "(Libre %d , (%.2f, %.2f))" (trouvenum a) (fst b) (snd b);
    Printf.printf "\n" ;
    printTest rest
;;

(*Question 3*)
(*Determine si deux quadTrees sont égaux *)
let sontEgaux qt1 qt2 = 
  match qt1, qt2 with
    Mur, Mur -> true
    |Libre(i), Libre(j) -> if i == j then true else false
    |_ -> false
;;
(* Determine les coordonnées du centre d'un Quad Libre dont le numéro est k *)
let donneCoordCenter qt k n =
    let lst = (tabCoordCenter qt n) in
    if (k-1) > (List.length lst)-1 then
      failwith "error"
    else begin
      let rec donneCoordCenter_aux lst = 
        match lst with
          [] -> failwith "La region que vous chercher n'existe pas !"
          |e::l -> if sontEgaux (fst e) (Libre(k)) then begin
                      snd (List.nth lst (List.length lst -1 - (k-1)));
                  end
                  else
                      donneCoordCenter_aux l;
                    
      in
      donneCoordCenter_aux lst;
    end
;;

(* Donne la distance entre deux Quads Libres en donnant en paramètre leurs numéros *)
let distEntreSommet qt r1 r2 n =
  let cc1 = donneCoordCenter qt r1 n in
  let cc2 = donneCoordCenter qt r2 n in
  sqrt(power2_float ((fst cc1)-.(fst cc2)) +. power2_float ((snd cc1)-.(snd cc2)))
;;

(* Dans le cas où une région est de taille > 1, alors cette fonction permet d'avoir toutes les coordonnées de la région *)
let lstCoordLibre x y dx dy t =
    let res = ref [ ] in
    for i = x to x+dx-1 do
        for j = y to y+dy-1 do
            add res (i, j, t)
        done;
    done;
    !res
;;
(* Lister toutes les coordonnées Libre *)
let listeTouteLesCoordsVides qt n =
  let res = ref [ ] in 
  let rec listeTouteLesCoordsVides_aux qt n cd =
    match qt with
      Mur -> ()
      |Libre(i) -> add res (lstCoordLibre (fst cd) (snd cd) n n i) 
      |Quad(a,b,c,d) -> listeTouteLesCoordsVides_aux a (n/2) (addCoordInt cd (0,(n/2))); listeTouteLesCoordsVides_aux b (n/2) (addCoordInt cd ((n/2),(n/2))); listeTouteLesCoordsVides_aux c (n/2) (addCoordInt cd (0, 0));listeTouteLesCoordsVides_aux d (n/2) (addCoordInt cd ((n/2), 0));
  in 
  listeTouteLesCoordsVides_aux qt n (0,0);
  List.concat(!res)
;;
(*Crée un tableau à partir d'un QuadTree*)
(*L'idée est que pour chaque coordonnée, celle-ci se voit attribuer le numéro de sa région*)
let creeTabQuad lst m =
  let rec tableau tab lst = 
    match lst with
      [] -> tab
      |(i,j,n)::l -> tab.(i).(j) <- n; tableau tab l
  in
  tableau (Array.make_matrix m m 0) lst
;;
(**
   Fonction de calcul des coordonnées des centres des régions libres.
   Renvoie un tableau de paires de coordonnées.
   
   Arguments :
   - (qt) le quadtree
   - (k) le nombre de régions libres
   - (n) la longueur du côté du terrain
   Pré-condition :
   - les régions doivent être numérotées de 0 à k-1
 *)
let mk_coords qt k t =
    let lst = ref [] in
    let n = Float.of_int t in
    let qqt = fst(qt) in
    let rec tabCoordCenter_aux qqt n cd = 
        match qqt with
            Mur -> ()
            |Libre(i) -> (let coordCenter = ((Float.div n 2.0), (Float.div n 2.0)) in
                        let coordPoint = 
                          let cc = cd in
                        addCoord coordCenter cc in
                        add lst (Libre(i), coordPoint))
            |Quad(a,b,c,d) -> tabCoordCenter_aux a (Float.div n 2.0) (addCoord cd (0.0,(n/.2.0))); tabCoordCenter_aux b (Float.div n 2.0) (addCoord cd ((n/.2.0),(n/.2.0))); tabCoordCenter_aux c (Float.div n 2.0) (addCoord cd (0.0,0.0)); tabCoordCenter_aux d (Float.div n 2.0) (addCoord cd ((n/.2.0),0.0)); 
    in
    tabCoordCenter_aux qqt n (0.0,0.0);
    !lst
;;

(*Crée un graphe à partir d'un tableau de Quadtree*)
(*- Après avoir créer mon tableau de QuadTree, je parcours toutes les cases du tableau et si 2 cases adjacantes sont differentes alors les deux régions sont voisines.
  - Je crée alors une liste où pour chaque région, je lui liste toutes ces régions voisines
  - Pour le tableau Graphe:
      - Sa taille correspond au nombre de régions totales
      - pour chaque case du tableau, il y a une liste de paire et avec pour chaque paire le numéro d'une région adjaçante et la distance les séparants
 *)

let mk_graph tab qt n = 
  let lst = ref [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i > 0 && tab.(i-1).(j) <> tab.(i).(j) && tab.(i-1).(j) <> 0 &&  tab.(i).(j) <> 0 then 
        add lst (tab.(i).(j), tab.(i-1).(j));
      if i < n-1 && tab.(i+1).(j) <> tab.(i).(j) && tab.(i+1).(j) <> 0 &&  tab.(i).(j) <> 0 then 
        add lst (tab.(i).(j), tab.(i+1).(j));
      if j > 0 && tab.(i).(j-1) <> tab.(i).(j) && tab.(i).(j-1) <> 0 &&  tab.(i).(j) <> 0  then 
        add lst (tab.(i).(j), tab.(i).(j-1));
      if j < n-1 && tab.(i).(j+1) <> tab.(i).(j) && tab.(i).(j+1) <> 0 &&  tab.(i).(j) <> 0  then 
        add lst (tab.(i).(j), tab.(i).(j+1));  
    done;
  done;
  let lst1, lst2 = List.split (!lst) in
  let l1 = List.rev(rem_from_left(List.sort compare lst1)) in (*La liste de chaque numéro de région libre *)
  let lstRes1 = ref [] in (*Une liste contenant un numéro de région libre et une liste de toutes ses régions adjacantes *)
  let nbTotalParlst = ref [] in (*Liste ou chaque indice représente une région et correspond au nombre de ses régions adjacantes*)
  let rec llll liste1 liste2 = 
    match liste1 with
      [] -> lstRes1
      |e::l -> let ll2 = List.find_all(fun x-> (fst x) == e) liste2 in
              let r1,r2 = List.split ll2 in
              add lstRes1 (e, (rem_from_left r2));
              add nbTotalParlst (List.length (rem_from_left r2));
              llll l liste2
  in
  lstRes1 := !(llll l1 (!lst));
  let t = List.length (!nbTotalParlst) in
  let g = Array.make (t) [] in (*Créer le tableau qui représentera le graphe *)
  let s1, s2 = List.split(!lstRes1) in (* S1.(i) la région et S2.(i) ses régions adjacantes *)
  for i = 0 to t-1 do
    let zz = List.nth (!nbTotalParlst) i in
    let lstFinal = ref [] in
    for j = 0 to zz-1 do
      let dist = distEntreSommet qt (List.nth s1 i) (List.nth (List.nth s2 i) j) n in 
      add lstFinal ((List.nth (List.nth s2 i) j), dist);
    done;
    g.(i) <- (!lstFinal);
  done;
  g
;;
(*Partie C*)
(*Question 1*)
(*Ajoute un élement dans une file et la trie*)
let addFile file x =
  add file x;
  let rec sort file =
    match file with 
      [] -> file
      |(a, b)::[] -> file
      |(a, b)::(c, d)::l -> if b < d then
                              (a,b)::sort((c, d)::l)
                            else
                              (c, d)::sort((a,b)::l)
  in
  file := sort (!file);
;;
(*Teste si la file est vide*)
let estVide file =
  match file with 
    [] -> true
    |_ -> false
;;
(*Retire le premier element de la file (qui est le plus prioritaire puisqu'il a été trié avec addFile)*)
let retirerElt file =
  let elt = remove file in
  elt
;;
(*Question 2*)
(*Algorithme de dijkstra qui pour chaque sommet donne la distance la plus courte à partir du point de départ*)
let dijkstra g s =
  let n = Array.length g in
  let dist = Array.make n infinity in
  let file_prio = ref [] in
  let ajoute s d = dist.(s-1) <- d ; addFile file_prio (s, d)  in
  ajoute s 0.0;
  while not (estVide (!file_prio)) do
    let ss, ds = retirerElt file_prio in
    List.iter(fun (v , dsv) -> let d = ds +. dsv in if d < dist.(v-1) then ajoute v d) g.(ss-1)
  done ;
  dist
;;
(*Tableau des prédécesseurs qui pour chaque sommet atteignable, donne son prédécesseur sur un chemin le plus court vers la source*)
let explore g s =
  let n = Array.length g in
  let preds = Array.make n (-1) in
  let vus = Array.make n false in
  let file = Queue.create () in
  let ajoute s p =
    Queue.add s file ;
    vus.(s-1) <- true ;
    preds.(s-1) <- p
  in
  ajoute s (-1);
  while not(Queue.is_empty file) do
    let ss = Queue.take file in
    vus.(ss-1) <- true ;
    List.iter (fun (v,x) -> if not vus.(v-1) then ajoute v ss) g.(ss-1)
  done ;
  preds
;;
(*Question 3*)
(*Determine le numéro de la région à partir de coordonnées*)
let determineRegion qt (x, y) n =
  let lstCoordVide = (listeTouteLesCoordsVides qt n) in
  let rec determineRegion_aux lst (s, t) =
    match lst with
      [] -> failwith "La coordonnee n'est pas valide !"
      |(a,b,i)::l -> if (a == s && b == t) then
                        i
                    else
                      determineRegion_aux l (s, t)
  in
  determineRegion_aux lstCoordVide (x,y)

;;
(*Liste le chemin (en numéro de de région) entre deux régions*)
let chemin g s c =
  let chemin = ref [] in
  let rec loop c =
    if s = c then 
      add chemin c
    else (
      add chemin c;
      loop g.(c-1)
    )
  in
  loop c;
  !chemin
;;
(**
   Fonction de recherche d'un chemin.
   Applique un algorithme de recherche et reconstruit le trouvé.
   Renvoie la liste des paires de coordonnées formant le chemin.

   Arguments :
   - (xDep, yDep) les coordonnées du point de départ
   - (xArr, yArr) les coordonnées du point d'arrivée
   - (qt, n) le quadtree et la longueur du côté du terrain
   - (g, coords) le graphe et le tableau des coordonnées
 *)
let find_path (xDep, yDep) (xArr, yArr) qt n g =
  let regionDepart = 
    if xDep == n && yDep <> n then
      determineRegion (fst(qt)) ((n-1), yDep) n 
    else if xDep <> n && yDep == n then
      determineRegion (fst(qt)) (xDep, (n-1)) n 
    else if xDep == n && yDep == n then
      determineRegion (fst(qt)) ((n-1), (n-1)) n 
    else
      determineRegion (fst(qt)) (xDep, yDep) n  in
  let regionArrive = 
    if xArr == n && yArr <> n then
      determineRegion (fst(qt)) ((n-1), yArr) n 
    else if xArr <> n && yArr == n then
      determineRegion (fst(qt)) (xArr, (n-1)) n 
    else if xArr == n && yArr == n then
      determineRegion (fst(qt)) ((n-1), (n-1)) n 
    else
      determineRegion (fst(qt)) (xArr, yArr) n 
  in
  let listePred = explore g regionDepart in
  let chemin = chemin listePred regionDepart regionArrive in
  let lst = ref [] in
  add lst (Int.to_float(xDep), Int.to_float(yDep));
  let rec faireChemin lstChemin =
    match lstChemin with
      [] -> !lst
      |e :: ll -> let coordActuelle = donneCoordCenter qt e n in
                  add lst coordActuelle;
                  faireChemin ll
  in
  lst := faireChemin chemin ;
  add lst (Int.to_float(xArr), Int.to_float(yArr));
  lst := rem_from_left(!lst) ;
  !lst
;;

let print_path p =
  let lst = List.rev p in
  List.iter (fun (x, y) -> printf "(%.2f, %.2f)\n" x y) lst;
  printf "\n"
;; 



(*Version optimisée*)
(*Algorithme A* *)
(*Vider une liste*)
let rec removeAll lst = 
  match lst with
  [] -> ()
  |[e] -> removeAll []
  |e::l -> removeAll l
;;
(*Ajouter un élément à une liste et la trier*)
let addFile2 file x =
  add file x;
  let rec sort file =
    match file with 
      [] -> file
      |(a, b, q,t)::[] -> file
      |(a, b, q, m)::(c, d, t,n)::l -> if b < d then
                              (a, b, q,m)::sort((c, d, t,n)::l)
                            else
                              (c, d, t,n)::sort((a, b, q,m)::l)
  in
  file := sort (!file);
;;
(*Fonction qui permet de relier les chemins (exemple: si un chemin 'a' a une destination bers un chemin 'b' alors il est relier au chemin suivant qui commence par 'b'*)
let cheminRes lst =
  let res = ref [] in
  let rec faire lst = 
    match lst with 
      [] -> !res
      |(a,b,c,d)::(e,f,g,h)::l -> if c == e then begin
                                    add res (a,b,c,d);
                                    add res (e,f,g,h);
                                    faire ((e,f,g,h)::l);
                                  end
                                  else
                                    faire ((a,b,c,d)::l);
      |(a,b,c,d)::[] -> !res
      
  in
  faire lst
;;
(*Filter pour ne garder que les numéro de région des chemins*)
let cheminRes2 lst =
  let res = ref [] in
  let rec faire lst = 
    match lst with 
      [] -> !res
      |(a,b,c,d)::l -> add res a;
                       faire l;
      
  in
  faire lst
;;
(*Algorithme A* : L'idée est d'additionner la distance euclidienne en le sommet actuel et le sommet d'arrivé à la distance entre chaque sommet et son voisin.*)
let aStar g qt (xDep, yDep) (xArr, yArr) n =
  let openList = ref [] in
  let closeList = ref [] in
  let sD =
    if xDep == n && yDep <> n then
      determineRegion (fst(qt)) ((n-1), yDep) n 
    else if xDep <> n && yDep == n then
      determineRegion (fst(qt)) (xDep, (n-1)) n 
    else if xDep == n && yDep == n then
      determineRegion (fst(qt)) ((n-1), (n-1)) n 
    else
      determineRegion (fst(qt)) (xDep, yDep) n 
  in 
  let sA = if xArr == n && yArr <> n then
      determineRegion (fst(qt)) ((n-1), yArr) n 
    else if xArr <> n && yArr == n then
      determineRegion (fst(qt)) (xArr, (n-1)) n 
    else if xArr == n && yArr == n then
      determineRegion (fst(qt)) ((n-1), (n-1)) n 
    else
      determineRegion (fst(qt)) (xArr, yArr) n 
  in 
  let distVolOiseau a b = distEntreSommet qt a b n in
  addFile2 openList (sD, 0., sD, 0.);
  while not (List.exists (fun (x,y,z,t) -> x == sA) (!closeList)) && not (estVide(!openList)) do
    add closeList (List.hd (!openList));
    let regActu, f, parent, distDj = remove openList in
    let rec voisins adj = 
      match adj with 
        [] -> ()
        |(d,c)::l ->  let dist1 = distVolOiseau d sA in
                      let dist12 =  c +. distDj in
                      let dist2 = dist1 +. dist12  in
                      if (List.exists (fun (x,y,z,t) -> x == d && y > dist2) (!openList)) then begin
                        (*openList := (List.filter (fun (x,y,z,t) -> x == d && y > dist2) (!openList));*)
                        addFile2 openList (d, dist2, regActu, dist12);
                        voisins l;
                      end
                      else if (List.exists (fun (x,y,z,t) -> x == d && y > dist2) (!closeList)) then begin
                        closeList := (List.filter (fun (x,y,z,t) -> x == d && y > dist2) (!closeList));
                        addFile2 openList (d, dist2, regActu, dist12);
                        voisins l;
                      end
                      else if not (List.exists (fun (x,y,z,t) -> x == d) (!closeList)) && not (List.exists (fun (x,y,z,t) -> x == d) (!openList)) then begin
                        addFile2 openList (d, dist2, regActu, dist12);
                        voisins l;
                      end
                      else 
                        voisins l;
    in
    voisins (g.(regActu-1));
  done;
  closeList := rem_from_left(cheminRes (!closeList));
  List.rev(cheminRes2(!closeList))
;;
let find_path2 g qt (xDep, yDep) (xArr, yArr) n =
  let pathAStar = aStar g qt (xDep, yDep) (xArr, yArr) n in
  let res = ref [] in
  add res (Float.of_int xDep, Float.of_int yDep);
  let rec ft lst =
    match lst with 
      [] -> add res (Float.of_int xArr, Float.of_int yArr); !res
      |e::l -> add res (donneCoordCenter qt e n); ft l
  in
  ft pathAStar
;;
(*Afficher le labyrinthe sous sa forme final sans les chemins tracés *)
let rec print_tab_string tab n =
  Printf.printf "\n";
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            let case = tab.(j).((n-1)-i) in
            if case < 10 then begin
              if case == 0 then begin
                Printf.printf "   \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                Printf.printf "   %s" (Int.to_string case);
                Printf.printf "\x1b[0m"; 
              end
            end
            else if case < 100 then begin
              if case == 0 then begin
                Printf.printf "  \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                Printf.printf "  %s" (Int.to_string case);
                Printf.printf "\x1b[0m"; 
              end
            end
            else begin
              if case == 0 then begin
                Printf.printf " \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                Printf.printf " %s" (Int.to_string case);
                Printf.printf "\x1b[0m"; 
              end
            end
        done;
    Printf.printf "\n";
    Printf.printf "\n";
    done;
    Printf.printf "\x1b[0m";
;;
(*Afficher le labyrinthe sous sa forme final avec les chemins tracés où juste le centre des régions est colorié *)
let tracePathQt1 g tab qt n = 
  let g1 = List.map (fun (x,y) -> (truncate(x),truncate(y))) g in

  Printf.printf "\n";
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            let case = tab.(j).((n-1)-i) in
            if case < 10 then begin
              if case == 0 then begin
                Printf.printf "   \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun (x,y) -> j == x && (n-1)-i == y) g1) then begin
                  Printf.printf "   \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                  Printf.printf "   %s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end
              end
            end
            else if case < 100 then begin
              if case == 0 then begin
                Printf.printf "  \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun (x,y) -> j == x && (n-1)-i == y) g1) then begin
                  Printf.printf "  \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                Printf.printf "  %s" (Int.to_string case);
                Printf.printf "\x1b[0m"; 
                end
              end
            end
            else begin
              if case == 0 then begin
                Printf.printf " \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun (x,y) -> j == x && (n-1)-i == y) g1) then begin
                  Printf.printf " \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                  Printf.printf " %s" (Int.to_string case);
                  Printf.printf "\x1b[0m"; 
                end
              end
            end
        done;
    Printf.printf "\n";
    Printf.printf "\n";
    done;
    Printf.printf "\x1b[0m";
;;
(*Afficher le labyrinthe sous sa forme final avec les chemins tracés où toute la région est colorié *)
let tracePathQt2 g tab qt n = 
  let g1 = List.map (fun (x,y) -> if truncate(x) < n && truncate(y) < n then (determineRegion qt (truncate(x),truncate(y)) n) else 1000) g in

  Printf.printf "\n";
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            let case = tab.(j).((n-1)-i) in
            if case < 10 then begin
              if case == 0 then begin
                Printf.printf "   \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun x -> x==case) g1) then begin
                  Printf.printf "   \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                  Printf.printf "   %s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end
              end
            end
            else if case < 100 then begin
              if case == 0 then begin
                Printf.printf "  \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun x -> x==case) g1) then begin
                  Printf.printf "  \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                Printf.printf "  %s" (Int.to_string case);
                Printf.printf "\x1b[0m"; 
                end
              end
            end
            else begin
              if case == 0 then begin
                Printf.printf " \226\172\154";
                Printf.printf "\x1b[0m"; 
              end
              else begin
                if (List.exists (fun x -> x==case) g1) then begin
                  Printf.printf " \x1B[41m%s" (Int.to_string case);
                  Printf.printf "\x1b[0m";
                end 
                else begin
                  Printf.printf " %s" (Int.to_string case);
                  Printf.printf "\x1b[0m"; 
                end
              end
            end
        done;
    Printf.printf "\n";
    Printf.printf "\n";
    done;
    Printf.printf "\x1b[0m";
;;
(*Chemins améliorés*)
(*Pour cette partie, je n'ai pas eu le temps de la réaliser mais mon approche consiste à ajouter une possibilité de déplacement en diagonal.
 De ce fait au lieu de se déplacer vers le haut puis vers la gauche par exemple, on pourrais calculer la distance entre les sommets diagonales et les sommets de manière horizontale et verticale puis comparer et choisir la plus courte *)

(*État donné que le 0 est considéré comme un mur, la nupérotation des régions libres est faites à partir de 1 *)
let _ =
  let file = Sys.argv.(1) in
  let murs, n = load file in
  (*L'itinéraire qui permet de voir la différence entre Dijkstra et A* est dep = (0,n/2) et arr = (n/2,n) *)
  let depart = (n/2, 0) in
  let arrivee = (n/2, n) in
(*
  (*Version simpliste*)
  (* Afficher le labyrinthe normal (0 pour le mur, 1 pour la zone libre) *)
  print_tab_int (creeLaby murs n) n ;
  Printf.printf "\n";
  let lll = parcours (creeLaby murs n) depart arrivee n in
  (*Afficher le labyrinthe avec l'itinéraire dessiné  (0 pour le mur, 1 pour la zone libre et 3 pour l'itinéraire) *)
  print_tab_int (trace_itin (creeLaby murs n) lll depart arrivee n) n;
 *)
  (*Version efficace et optimisée*)
  let t1 = Unix.gettimeofday() in
  let qt = numerote (list2qtrees n murs) 1 in
  let t2 = Unix.gettimeofday() in
  let tab = creeTabQuad (listeTouteLesCoordsVides (fst(qt)) n) n in
  let g = mk_graph tab qt n in
  let t3 = Unix.gettimeofday() in
  let path = find_path depart arrivee qt n g in
  let t4 = Unix.gettimeofday() in
  let path2 = find_path2 g qt depart arrivee n in
  let t5 = Unix.gettimeofday() in
  printf "\nTemps:\nConstruction du quadtree : %fs\nConstruction du graphe : %fs\nRecherche de chemin en Dijsktra : %fs\nRecherche de chemin en A * :  %fs\n" (t2 -. t1) (t3 -. t2) (t4 -. t3) (t5 -. t4) ;
  Printf.printf "\nReprésentations :\n";
  Printf.printf "Chemin en QuadTree avec toute la région avec Dijkstra :  \n";
  tracePathQt2 path (creeTabQuad (listeTouteLesCoordsVides (fst(qt)) n) n) (fst(qt)) n ;
  Printf.printf "Chemin en QuadTree avec centre de chaque région avec Dijkstra :  \n";
  tracePathQt1 path (creeTabQuad (listeTouteLesCoordsVides (fst(qt)) n) n) (fst(qt)) n ;
  Printf.printf "Chemin en QuadTree avec toute la région en A* :  \n";
  tracePathQt2 path2 (creeTabQuad (listeTouteLesCoordsVides (fst(qt)) n) n) (fst(qt)) n ;
  Printf.printf "Chemin en QuadTree avec centre de chaque région avec A* :  \n";
  tracePathQt1 path2 (creeTabQuad (listeTouteLesCoordsVides (fst(qt)) n) n) (fst(qt)) n ;
  Printf.printf "Coordonnées avec Dijkstra : \n";
  print_path path;
  Printf.printf "Coordonnées avec A * : \n";
  print_path path2;
;;
