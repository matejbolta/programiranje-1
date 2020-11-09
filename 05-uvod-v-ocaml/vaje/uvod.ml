
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

let square n = n * n

let square' = (fun x -> x * x)

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

(* Namesto _ lahko pišemo tudi _x oz _ime *)
let middle_of_triple (_, y, _) = y

let middle_of_triple' triple =
  let (_, y, _) = triple in
  y

let middle_of_triple'' triple = match triple with (* let middle_of_triple'' = function *)
  | (_, y, _) -> y

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

(*
let starting_element sez =
  let x :: _ = sez in
  x
*)

let starting_element' sez =
  match sez with
  | [] -> failwith "Podal si prazen seznam" (* Lahko tudi assert false *)
  | x :: _ -> x

let starting_element'' = function
  | [] -> failwith "Podal si prazen seznam" (* Lahko tudi assert false *)
  | x :: _ -> x

(* Druga ali tretja opcija sta uredu *)

(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo (1).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply = function
  | [] -> 1
  | x :: xs -> x * multiply xs

(* Repno rekurzivna, moj dodatek *)
let multiply_repna xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | x :: xs' -> aux (x * acc) xs'
  in
  aux 1 xs

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs = function
  | [] -> []
  | x :: xs -> (
    let (prvi, drugi) = x in
    (prvi + drugi) :: sum_int_pairs xs
  )

(* x lahko že takoj razstavimo v (prvi, drugi) in izpustimo let in *)
let rec sum_int_pairs' = function
  | [] -> []
  | (x1, x2) :: xs -> (x1 + x2) :: (sum_int_pairs xs)

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k list =
  if k <= 0 then
    match list with
    | [] -> failwith "List too short."
    | x :: xs -> x
  else (* k > 0 *)
    match list with
    | [] -> failwith "List too short."
    | x :: xs -> get (k - 1) xs

(* Bolje oz edino pravilno *)
let rec get' k list = match list with (* let rec get k = function *)
  | [] -> failwith "List too short."
  | x :: xs -> (if k <= 0 then x else get (k - 1) xs)

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x :: xs -> x :: x :: double xs (* | x :: xs -> [x; x] @ double xs *)

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k = function (* Naredi match na list *)
  | [] -> [x] (* k je nepomemben, ker je zmanjkalo seznama *)
  | y :: ys -> (
    if k <= 0 then x :: y :: ys
    else y :: insert x (k - 1) ys
  )

(* Malo ekstra *)
let rec insert' x k = function
  | [] -> x :: []
  | y :: ys when k <= 0 -> x :: y :: ys
  | y :: ys (* when k > 0 *) -> y :: insert x (k - 1) ys

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

(* To ne deluje za negativne k-je *)
let rec divide k list = match (k, list) with
  | (0, xs) -> ([], xs)
  | (_, []) -> ([], [])
  | (k', x :: xs) ->
    let (prvi, drugi) = divide (k' - 1) xs in
    (x :: prvi, drugi)

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list = match (n, list) with
  | (0, xs) -> xs
  | (_, []) -> failwith "Empty list given"
  | (n, x :: xs) -> rotate (n - 1) (xs @ [x])

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list = match list with
  | [] -> []
  | y :: ys -> if y = x then remove x ys else y :: remove x ys

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let is_palindrome list =
  let rec obrni = function
    | [] -> []
    | x :: xs -> (obrni xs) @ [x]
  in
  list = obrni list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components l1 l2 = match (l1, l2) with
  | ([], _) -> []
  | (_, []) -> []
  | (x :: xs, y :: ys) -> (if x >= y then x else y) :: max_on_components xs ys

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let second_largest list =
  let rec largest = function
    | [] -> failwith "Empty list."
    | x :: [] -> x
    | x :: xs -> if x >= largest xs then x else largest xs
  in
  let list1 = remove (largest list) list in
  largest list1
