(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)
type euro = Euro of float
type dollar = Dollar of float

let euro_to_dollar (Euro x) = Dollar (1.18 *. x)

let dollar_to_euro (Dollar x) = Euro (0.84 *. x)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
        ...če imate maca :(
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)
type currency =
  | Yen of float
  | Pound of float
  | Krona of float

let to_pound = function
  | Yen x -> Pound (0.0072 *. x)
  | Pound x -> Pound x
  | Krona x -> Pound (0.088 *. x)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)
type intbool_list =
  | Nil
  | Int of int * intbool_list
  | Bool of bool * intbool_list

let test = Int(5, Bool(true, Bool(false, Int(7, Nil))))

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = match ib_list with
  | Int(x, tail) -> Int(f_int x, intbool_map f_int f_bool tail)
  | Bool(x, tail) -> Bool(f_bool x, intbool_map f_int f_bool tail)
  | Nil -> Nil
(* intbool_map ((+) 2) ((&&) true) test;; *)

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse intbool_list =
  let rec aux acc = function
  | Nil -> acc
  | Int(x, tail) -> aux (Int(x, acc)) tail
  | Bool(b, tail) -> aux (Bool(b, acc)) tail
  in
  aux Nil intbool_list

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let reverse list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] list

let intbool_separate ib_list =
  let rec aux acc_int acc_bool = function
  | Nil -> reverse acc_int, reverse acc_bool
  | Int(x, tail) -> aux (x :: acc_int) acc_bool tail
  | Bool(b, tail) -> aux acc_int (b :: acc_bool) tail
  in
  aux [] [] ib_list
  (* Funkcijo lahko kličemo tudi
  aux [] [] intbool_reverse ib_list
  in pozabimo naš običajen reverse *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane

type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status =
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = {name: string; status: status}

let professor = {name = "Matija"; status = Employed(Fire, Teacher)}
let me = {name = "Matej"; status = Student(Arcane, 2)}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int}

let update counter magic =
  let {fire = x; frost = y; arcane = z} = counter in
  match magic with
  | Fire -> {fire = (x + 1); frost = y; arcane = z}
  | Frost -> {fire = x; frost = (y + 1); arcane = z}
  | Arcane -> {fire = x; frost = y; arcane = (z + 1)}

(* Malo drugačna (boljša) sintaksa:
let update counter' = function
  | Fire -> {counter with fire = counter.fire + 1}
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1} *)

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let count_magic list =
  let rec aux counter = function
    | [] -> counter
    | {name; status = Newbie} :: wizards
      -> aux counter wizards
    | {name; status = Student(magic, _)} :: wizards
      -> aux (update counter magic) wizards
    | {name; status = Employed(magic, _)} :: wizards
      -> aux (update counter magic) wizards
  in
  aux {fire = 0; frost = 0; arcane = 0} list

(* Malo drugačno matchanje da pridemo do magic *)
let count_magic' list =
  let rec aux counter = function
    | [] -> counter
    | wizard :: wizards ->
      match wizard.status with
        | Newbie -> aux counter wizards
        | Student(magic, _) | Employed(magic, _) -> aux (update counter magic) wizards
  in
  aux {fire = 0; frost = 0; arcane = 0} list

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list =
  let year = match specialisation with
   | Historian -> 3
   | Researcher -> 4
   | Teacher -> 5
  in
  match wizard_list with
  | [] -> None
  | wizard :: wizards ->
    match wizard.status with
      | Student(magic', year') when year' >= year && magic' = magic -> Some wizard.name
      | _ -> find_candidate magic specialisation wizards

(*
Spodnji del lahko naredimo tudi z pomožno funkcijo
  let rec search = function
    | [] -> None
    | {name; status} :: wizards ->
        match status with
        | Student (m, y) when m = magic && y >= year -> Some name
        | _ -> search wizards
  in
  search wizard_list
*)