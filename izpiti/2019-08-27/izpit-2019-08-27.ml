let odstej_trojici (x, y, z) (a, b, c) = x - a, y - b, z - c

let max_rezultat_do_n f n = (* lepo gre z List.init in fold_left *)
  (* tole je lepo *)
  (* if n = 0 then f 0 else max (f n) (max_rezultat_do_n f (n-1)) *)
  let rec aux acc m =
    match m with
    | m when m > n -> acc
    | m ->
      if f m > acc then aux (f m) (m + 1)
      else aux acc (m + 1)
  in
  aux (f 0) 1
(*  *)

let pocisti_seznam l =
  let rec aux acc = function
    | [] -> List.rev acc
    | None :: tail -> aux acc tail
    | Some x :: tail -> aux (x :: acc) tail
  in
  aux [] l
(*  *)

let preveri_urejenost l =
  let rec razkosaj sode lihe = function
    | [] -> List.rev sode, lihe (* lihe namenoma neobrnjene *)
    | x :: xs when x mod 2 = 0 -> razkosaj (x :: sode) lihe xs
    | x :: xs -> razkosaj sode (x :: lihe) xs
  in
  let rec urejen = function
    | [] | _ :: [] -> true
    | x1 :: x2 :: tail ->
      if x1 < x2 then urejen (x2 :: tail)
      else false
  in
  let sode, lihe = razkosaj [] [] l in
  urejen sode && urejen lihe
(*  *)

(* --------------------------------------------------------------------- *)

type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list
(*  *)

let primer = [Element 1; Element 2; Podseznam [Element 3; Podseznam [Element 4]; Podseznam []]; Podseznam [Element 5]]

let rec najvecja_globina l =
  match l with
  | [] -> 0
  | Element _ :: tail -> max 1 (najvecja_globina tail)
  | Podseznam p :: tail -> max (1 + najvecja_globina p) (najvecja_globina tail)
(*  *)

let rec preslikaj f l =
  match l with
  | [] -> []
  | Element x :: tail -> Element (f x) :: preslikaj f tail
  | Podseznam p :: tail -> Podseznam (preslikaj f p) :: preslikaj f tail
(*  *)

let rec splosci l =
  match l with
  | [] -> []
  | Element x :: tail -> x :: splosci tail
  | Podseznam p :: tail -> splosci p @ splosci tail
(*  *)

let rec alternirajoci_konstruktorji l =
  match l with
  | [] | _ :: [] -> true
  | Element _ :: Podseznam p :: tail -> alternirajoci_konstruktorji ((Podseznam p) :: tail)
  | Podseznam _ :: Element x :: tail -> alternirajoci_konstruktorji ((Element x) :: tail)
  | _ -> false
(*  *)

(* tole spodaj je tezje, letos nismo delali *)
let rec zlozi_preko_gnezdenja f acc sez = sez |> splosci |> List.fold_left f acc (* splosci ni repno rekurziven *)
(*  *)
let rec zlozi_preko_gnezdenja_tl_rec f acc sez = (* ubistvu implementacija fold_left za take gnezdene tipe *)
  let rec zlozi acc (todo_list : 'a gnezdenje list list) (sez : 'a gnezdenje list) =
    match sez with
    | [] ->
      begin match todo_list with
      | [] -> acc
      | task :: tasks ->
        zlozi acc tasks task
      end
    | Element x :: tail ->
      zlozi (f acc x) todo_list tail
    | Podseznam sez :: tail ->
      zlozi acc (tail :: todo_list) sez
  in
  zlozi acc [] sez
(*  *)