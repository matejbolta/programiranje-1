(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list (kot listek, ne seznam).
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree_no_leaf = Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Node (Node (Empty, 6, Empty), 7, Node (Empty, 11, Empty)))

let leaf x = Node (Empty, x, Empty)

let test_tree = Node (Node (leaf 0, 2, Empty), 5, Node (leaf 6, 7, leaf 11))

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror tree = match tree with
  | Empty -> Empty
  | Node (l, x, d) ->
    let l', d' = mirror l, mirror d in
    Node(d', x, l')

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height tree = match tree with
  | Empty -> 0
  | Node (l, _, d) -> 1 + max (height l) (height d)

let rec size tree = match tree with
  | Empty -> 0
  | Node (l, _, d) -> 1 + size l + size d

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree = match tree with
 | Empty -> Empty
 | Node (l, x, d) ->
    let l, d = map_tree f l, map_tree f d in
    Node (l, f x, d)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, d) -> list_of_tree l @ [x] @ list_of_tree d

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let is_bst tree =
  let rec ascending_list = function
    | [] | _ :: [] -> true
    | x :: y :: tail ->
      if x > y then false else ascending_list (y :: tail)
  in
  tree |> list_of_tree |> ascending_list
(* Uradna actually ista :o :) *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x tree = match tree with
  | Empty -> leaf x
  | Node (l, x', d) ->
    if x < x' then Node (insert x l, x', d)
    else if x > x' then Node (l, x, insert x d)
    else tree

let rec member2 x tree = match tree with
  | Empty -> false
  | Node (l, x', d) ->
    if x' = x then true
    else member2 x l || member2 x d
(* ali celo trojni || pogoj, x' = x dodamo noter *)

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

(* Točno to sem jaz napisu v prejšnji nalogi, opsala *)
(* Pa dajmo zdej napisat tale hitrejši member, ki predpostavi BST *)
let rec member x tree = match tree with
  | Empty -> false
  | Node (l, x', d) ->
    if x' = x then true
    else if x < x' then member x l
    else (* x > x' *) member x d

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ tree = match tree with
  | Empty -> None
  | Node (_, _, d) ->
    let list = list_of_tree d in
    match list with
    | [] -> None
    | x :: _ -> Some x

let reverse list =
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (x :: acc) xs
  in
  aux [] list

let pred tree = match tree with
  | Empty -> None
  | Node (l, _, _) ->
    let list = reverse (list_of_tree l) in
    match list with
    | [] -> None
    | x :: _ -> Some x

(* Uradne to delajo z pomožnima funkcijama minimal in maximal *)

(* let succ bst =
  let rec minimal = function
    | Empty -> None
    | Node(Empty, x, _) -> Some x
    | Node(l, _, _) -> minimal l
  in
  match bst with
  | Empty -> None
  | Node(_, _, r) -> minimal r *)

(* let pred bst =
  let rec maximal = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, r) -> maximal r
  in
  match bst with
  | Empty -> None
  | Node(l, _, _) -> maximal l *)

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let delete x tree =
  match tree with
  | Empty -> Empty
  | _ -> Empty (* TODO *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = Node (leaf ("a", 0), ("b", 1), Node (leaf ("c", -2), ("d", 2), Empty))
(* let test_dict : (string, int) dict = ... *)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key dict = match dict with
  | Node (_, (k, v), _) when k = key -> Some v
  | Node (l, (k, _), _) when k < key -> dict_get key l
  (* | Node (_, _, d) -> dict_get key d *)
  | Node (_, (k, _), d) when k > key -> dict_get key d
  | _ -> None
 
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict (dict: (string, int) dict) = match dict with
  | Empty -> ()
  | Node (l, (k, v), d) ->
    print_dict l; (* ; vzame 2 unit expresiona in izvede oba, vrne unit *)
    print_endline (k ^ " : " ^ string_of_int v);
    (* print_string (k ^ " : "); print_int v; print_newline (); *)
    print_dict d

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value dict = match dict with
  | Empty -> leaf (key, value)
  | Node (l, (k, v), d) ->
    if k = key then Node (l, (key, value), d)
    else if key < k then Node (dict_insert key value l, (k, v), d)
    else Node (l, (k, v), dict_insert key value d)
