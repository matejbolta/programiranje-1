(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq m n = m = n (* let eq = (=) *)
  let zero = 0
  let one = 1
  let add m n = m + n (* let add = (=) *)
  let sub m n = max 0 (m - n)
  let mul m n = m * n (* let mul = ( * ) *)
  let to_int m = m
  let of_int m = m
end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Zero | Succ of t
  let rec eq m n = match m, n with
    | Zero, Zero -> true
    | Zero, _  | _, Zero -> false
    | Succ m, Succ n -> eq m n
  let zero = Zero
  let one = Succ Zero
  let rec add m n = match m, n with
    | m, Zero -> m
    | m, Succ n -> Succ (add m n)
  let rec sub m n = match m, n with
    | m, Zero -> m
    | Zero, n -> Zero
    | Succ m, Succ n -> sub m n
  let rec mul m n = match m, n with
    | Zero, _ -> Zero
    | Succ m, n -> add (mul m n) n
  let rec of_int m = match m with
    | m when m <= 0 -> Zero
    | m -> Succ (of_int (m - 1))
  let rec to_int m = match m with
    | Zero -> 0
    | Succ m -> 1 + to_int m
end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) =
  let rec aux sum border = match border with
    | n when Nat.eq n Nat.zero -> sum |> Nat.to_int
    | number -> aux (Nat.add sum (Nat.sub number Nat.one)) (Nat.sub number Nat.one)
  in
  aux Nat.zero (Nat.of_int 100)

(* Uradna, podobno *]
let sum_nat_100 (module Nat : NAT) =
  let hundred = Nat.of_int 100 in
  let rec sum current acc =
    if Nat.eq current hundred then
      acc
    else
      sum (Nat.add current Nat.one) (Nat.add acc current)
  in
  sum Nat.zero Nat.zero |> Nat.to_int
[* *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t
  val one : t
  val i : t
  val neg : t -> t
  val conj : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re = 0.; im = 1.}
  let neg t = {re = -. t.re; im = -. t.im}
  let conj t = {t with im = -. t.im}
  let add t1 t2 = {re = t1.re +. t2.re; im = t1.im +. t2.im}
  let mul t1 t2 = {
    re = (t1.re *. t2.re -. t1.im -. t2.im);
    im = (t1.re *. t2.im +. t1.im *. t2.re)
  }
end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq x y =
    x.magn = y.magn &&
      (x.magn = 0. || (mod_float x.arg 360. = mod_float y.arg 360.))
  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 90.}
  let neg {magn; arg} = {magn; arg = arg +. 180.}
  let conj {magn; arg} = {magn; arg = (mod_float (arg +. 180.) 360.)}
  let mul x y = {magn = x.magn *. y.magn ; arg = x.arg +. y.arg}

  (* Rešitve (za vsoto): *)
  let re {magn; arg} = magn *. cos (rad_of_deg arg)
  let im {magn; arg} = magn *. sin (rad_of_deg arg)

  let arg re im =
    let rad =
      if re > 0. then atan (im /. re)
      else if re < 0. && im >= 0. then atan (im /. re) +. pi
      else if re < 0. && im < 0. then  atan (im /. re) -. pi
      else if re = 0. && im > 0. then pi /. 2.
      else if re = 0. && im < 0. then -.(pi /. 2.)
      else 0.
    in deg_of_rad rad

  let magn re im = sqrt (re *. re +. im *. im)

  let add x y =
    let square x = x *. x in
    let magn = sqrt (square x.magn +. square y.magn +. 2. *. x.magn *. y.magn *. cos (y.arg -. x.arg))
    and arg = x.arg +.
                atan2 (y.magn *. sin (y.arg -. x.arg))
                      (x.magn +. y.magn *. cos (y.arg -. x.arg)) in
    {magn; arg}

  let add' x y =
    let z_re, z_im = re x +. re y, im x +. im y in
    let arg = arg z_re z_im
    and magn = magn z_re z_im
    in {arg; magn}
    (* To vse je bilo za vsoto *)
end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)

module type DICT = sig
  type ('key, 'value) t

  val empty : ('key, 'value) t

  val dict_get : 'key -> ('key, 'value) t -> 'value option
  val dict_insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val print_dict : (string, int) t -> unit
end

module Tree_dict : DICT = struct
  type ('key, 'value) t = Empty | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = Empty
  let leaf key value = Node (Empty, key, value, Empty)
  let rec dict_get key = function
    | Empty -> None
    | Node (l, k, v, d) ->
      if k = key then Some v
      else if key < k then dict_get key l
      else dict_get key d
  let rec dict_insert key value = function
   | Empty -> leaf key value
   | Node (l, k, v, d) ->
    if k = key then Node (l, k, value, d)
    else if key < k then Node (dict_insert key value l, k, v, d)
    else Node (l, k, v, dict_insert key value d)
  
  let rec print_dict = function
  | Empty -> ()
  | Node (l, k, v, d) ->
    print_dict l;
    print_string (k ^ " : "); print_int v; print_string "\n";
    print_dict d
end
(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list =
  let rec counter dict = function
    | [] -> Dict.print_dict dict
    | x :: xs ->
      let new_count =
        match Dict.dict_get x dict with
        | None -> 1
        | Some x -> x + 1
      in
      let new_dict = Dict.dict_insert x new_count dict in
      counter new_dict xs
  in
  counter Dict.empty list