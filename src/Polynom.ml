(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- POLYNOM PACKAGE --- Polynomials manipulation. Their coefficients are of type Numerix (coefficients on ---- == *)
(* ==                      --- the Real or Complex field).                                                       ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: Polynom.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

(* ===== POLYNOMIAL-TYPE DEFINITION ===== *)
type monom == (numerix * int);;
type poly = NULL | P of (monom * poly);;
(* Poly :  (coeff, pow > b) ---- (coeff, pow = b) ---- (coeff, pow < b)
   Coeffs are different from zero. If they are null, they disappear. *)

let monom_eval (m:monom) (n:numerix) = (mult_numx (fst(m)) (power_numx n (snd(m))));;

(* ================================== *)
(* == Print polynomials Functions. == *)
(* ================================== *)
#open "format";;

let print_monom (m : monom) = match m with
  | (R(Int(0)), _)         -> ();
  | (C(Int(0), Int(0)), _) -> ();
  | (a, 0)                 -> begin match a with
                                | C(_) -> print_string "("; print_numx a; print_string ")";
                                | _    -> print_numx a;
                              end;
  | (a, 1)                 -> begin match a with
                                | C(_) -> print_string "("; print_numx a; print_string ")";
                                |_     -> print_numx a;
                              end;
                              print_string " x";
  | (a, pow)               -> begin match a with
                                | C(_) -> print_string "("; print_numx a; print_string ")";
                                | _    -> print_numx a;
                              end;
                              print_string " x^"; print_int pow;
;;

let rec print_poly (p : poly) = match p with
  | NULL       -> print_string "00";
  | P(m, NULL) -> print_monom m;
  | P(m, poly) -> print_monom m; print_string " + "; print_poly poly;
;;

(*let rec print_poly (p : poly) = match p with
   | NULL                  -> print_string "00";
   | P((coeff, pow), poly) -> print_numx coeff; print_string " x^"; print_int pow; print_string " + "; print_poly poly;
;;*)

(*let print_poly (p:poly) =
  (* Impression d'un monôme sans le signe. *)
  let intern_print_monom m =
    let coeff = normalize_numx (fst(m)) and pow = snd(m) in
    
    if (coeff <>% R(Int(0))) then begin
      if (abs2 coeff =% R(Int(1))) then begin
        if ((coeff =% C(Int(0), Int(1))) || (coeff =% C(Int(0), Int(-1)))) then print_numx (C(Int(0), Int(1)));
      end
      else if ((isReal coeff) || (isImg coeff)) then
        print_numx (abs_numx coeff)
      else begin
        print_string "(";
        print_numx coeff;
        print_string ")";
      end;
      
      print_string " x";
      if (pow >= 2) then print_string ("^" ^ (string_of_int pow));
    end;
  in
  
  let rec intern_print_poly polynom = match polynom with
    |NULL -> print_string "00";
    |P(m, NULL) -> intern_print_monom m;
    |P(m, poly) -> intern_print_monom m;
  in
  
  intern_print_poly p;
;;*)

(*let rec print_poly (p:poly) = match p with
  |NULL -> print_string "00";
  |P((R(Int(0)), pow), poly) -> print_poly poly;
  |P((coeff, 0), NULL) -> print_numx coeff;
  |P((coeff, 1), NULL) -> print_numx coeff; print_string " x";
  |P((coeff, pow), NULL) -> print_numx coeff; print_string " x^"; print_int pow;
  |P((coeff, 0), poly) -> print_numx coeff; print_string " + "; print_poly poly;
  |P((coeff, 1), poly) -> print_numx coeff; print_string " x + "; print_poly poly;
  |P((coeff, pow), poly) -> print_numx coeff; print_string " x^"; print_int pow; print_string " + "; print_poly poly;
;;*)

install_printer "print_monom";;
install_printer "print_poly";;
(* =============================== *)

(* ====================================================================== *)
(* == Functions for Merge-sorting of monomials, regarding their power. == *)
(* ====================================================================== *)
let rec poly_cut (p : poly) = match p with
  | NULL               -> (NULL, NULL);
  | P(m, NULL)         -> (p, NULL);
  | P(m1, P(m2, poly)) -> let (p1, p2) = (poly_cut poly) in (P(m1, p1), P(m2, p2));
;;

let rec poly_merge (p1 : poly) (p2 : poly) = match (p1, p2) with
  | (NULL, _)                                                               -> p2;
  | (_, NULL)                                                               -> p1;
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 > pow2) -> P((coeff1, pow1), (poly_merge poly1 p2));
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 = pow2) ->
      let coeff = (add_numx coeff1 coeff2) in
      if (not(eq_numx coeff (R(Int(0))))) then
        P((coeff, pow1), (poly_merge poly1 poly2))
      else
        (poly_merge poly1 poly2);
  | (P(m1, poly1), P(m2, poly2)) (* when (pow1 < pow2) *)                   -> P(m2, (poly_merge p1 poly2));
;;

let rec poly_merge_sort (p : poly) = match p with
  | NULL       -> NULL;
  | P(m, NULL) -> p;
  | _          -> let (p1, p2) = (poly_cut p) in (poly_merge (poly_merge_sort p1) (poly_merge_sort p2));
;;
(* =================================================================== *)

let rec list_of_poly (p : poly) = match p with
  | NULL                      -> [];
  | P((R(Int(0)), pow), poly) -> list_of_poly poly;
  | P(m, poly)                -> m::(list_of_poly poly);
;;

(* We suppose that the list is down-sorted regarding the powers. *)
let rec poly_of_numxList (l : monom list) = match l with
  | []                         -> NULL;
  | (_, pow)::q when (pow < 0) -> poly_of_numxList q;
  | (R(Int(0)), pow)::q        -> poly_of_numxList q;
  | m::q                       -> P(m, (poly_of_numxList q));
;;
let rec poly_of_list (l : (float * int) list) = match l with
  | []                         -> NULL;
  | (_, pow)::q when (pow < 0) -> poly_of_list q;
  | (0., pow)::q               -> poly_of_list q;
  | (coeff, pow)::q            -> P(((numerix_of_float coeff), pow), (poly_of_list q));
;;

(* ============================ *)
(* == Clean the polynomials. == *)
(* ============================ *)
(* Procedure which erases null coefficients, and sorts monomials regarding to their power. *)
let rec fire_zeros (poly : poly) = match poly with
  | NULL                      -> NULL;
  | P((R(Int(0)), pow), poly) -> fire_zeros poly;
  | P(m, poly)                -> P(m, (fire_zeros poly));
;;
let poly_cleaning (p : poly) = (poly_merge_sort (fire_zeros p));;
(* ================================ *)


(* ==================================================================================== *)
(* == INTERNALS FUNCTIONS -                                                          == *)
(* == POLYNOMIALS MANIPULATION: WE SUPPOSE THAT THE POLYNOMIALS ARE ALREADY CLEANED. == *)
(* == RETURN CLEANED POLYNOMIALS.                                                    == *)
(* ==================================================================================== *)
let rec intern_poly_minus (p : poly) = match p with
  | NULL                  -> NULL;
  | P((coeff, pow), poly) -> P((neg_numx coeff, pow), (intern_poly_minus poly));
;;

let rec intern_poly_add (p1 : poly) (p2 : poly) = match (p1, p2) with
  | (_, NULL)                                                               -> p1;
  | (NULL, _)                                                               -> p2;
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 > pow2) -> P((coeff1, pow1), (intern_poly_add poly1 p2));
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 = pow2) ->
      let coeff = (add_numx coeff1 coeff2) in
      if (not(eq_numx coeff (R(Int(0))))) then
        P((coeff, pow1), (intern_poly_add poly1 poly2))
      else
        (intern_poly_add poly1 poly2);
  | (P(m1, poly1), P(m2, poly2)) (* when (pow1 < pow2) *)                   -> P(m2, (intern_poly_add p1 poly2));
;;

let rec intern_poly_sub (p1 : poly) (p2 : poly) = match (p1, p2) with
  | (_, NULL)                                                                     -> p1;
  | (NULL, _)                                                                     -> intern_poly_minus p2;
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 > pow2)       -> P((coeff1, pow1), (intern_poly_sub poly1 p2));
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 = pow2)       ->
      let coeff = (sub_numx coeff1 coeff2) in
      if (not(eq_numx coeff (R(Int(0))))) then
        P((coeff, pow1), (intern_poly_sub poly1 poly2))
      else
        (intern_poly_sub poly1 poly2);
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) (* when (pow1 < pow2) *) -> P(((neg_numx coeff2), pow2), (intern_poly_sub p1 poly2));
;;

(* 'Bâtard' (pardon my French!!) algorithm for multiplying two polynomials. Improved version: see Donald KNUTH. *)
let rec intern_poly_mult (p1 : poly) (p2 : poly) = match (p1, p2) with
  | (_, NULL)                                            -> NULL;
  | (NULL, _)                                            -> NULL;
  | (P((coeff1, pow1), NULL), P((coeff2, pow2), poly))   -> P(((mult_numx coeff1 coeff2), pow1 + pow2), (intern_poly_mult p1 poly));
  | (P((coeff1, pow1), poly), P((coeff2, pow2), NULL))   -> P(((mult_numx coeff1 coeff2), pow1 + pow2), (intern_poly_mult p2 poly));
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) -> (intern_poly_add (intern_poly_add (P(((mult_numx coeff1 coeff2), pow1+pow2), NULL)) (intern_poly_add (intern_poly_mult (P((coeff1, pow1), NULL)) poly2) (intern_poly_mult (P((coeff2, pow2), NULL)) poly1))) (intern_poly_mult poly1 poly2));
;;

let rec intern_poly_derivate (p : poly) = match p with
  | NULL                  -> NULL;
  | P((coeff, 0), poly)   -> NULL;
  | P((coeff, pow), poly) -> P(((mult_numx coeff (R(Int(pow)))), pow - 1), (intern_poly_derivate poly));
;;

let rec intern_poly_integrate (p : poly) = match p with
  | NULL                  -> NULL;
  | P((coeff, pow), poly) -> P(((div_numx coeff (R(Int(pow+1)))), pow + 1), (intern_poly_integrate poly));
;;

(* See polynomial long division euclidean algorithm on the Internet. *)
let rec intern_eucl_div (num : poly) (den : poly) = match (num, den) with
  | (NULL, NULL) -> (NULL, NULL);
  | (_, NULL)    -> failwith "**** !! ERROR in 'intern_eucl_div' -- division by a null polynomial !! ****";
  | (NULL, _)                                                                      -> (NULL, NULL);
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) when (pow1 < pow2)        -> (NULL, num);
  | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) (* when (pow1 >= pow2) *) ->
      let m = ((div_numx coeff1 coeff2), pow1 - pow2) in
      let (quot, remaind) = (intern_eucl_div (intern_poly_sub poly1 (intern_poly_mult (P(m, NULL)) poly2)) den) in
      (P(m, quot), remaind);
;;

let rec intern_poly_power (p : poly) (pow : int) = match pow with
  |a when (a < 0) -> failwith "**** !! ERROR in 'intern_poly_power' -- negative power !! ****";
  |0              -> P((R(Int(1)), 0), NULL);
  |1              -> p;
  |a              -> let q = (intern_poly_power p (a / 2)) in
                     if (a mod 2 = 0) then
                       (intern_poly_mult q q)
                     else
                       (intern_poly_mult p (intern_poly_mult q q));
;;

let intern_poly_degree (p : poly) = match p with
  | NULL                  -> Infinity(false);
  | P((coeff, pow), poly) -> R(Int(pow));
;;

let intern_lead_coeff (p : poly) = match p with
  | NULL                  -> R(Int(0));
  | P((coeff, pow), poly) -> coeff;
;;

let intern_poly_gcd (p1 : poly) (p2 : poly) =
  let q = ref p1
  and r = ref p2 in
  
  let temp = ref NULL in
  while (!r <> NULL) do
    temp := !q;
    q := !r;
    r := snd(intern_eucl_div !temp !r);
  done;
  
  !q;
;;
(* ==================================================================================== *)



(* =============================== *)
(* == PUBLIC FUNCTIONS -        == *)
(* == POLYNOMIALS MANIPULATION. == *)
(* =============================== *)
let rec poly_mult_by_numx (c : numerix) (p : poly) = match p with
  | NULL                  -> NULL;
  | P((coeff, pow), poly) -> P(((mult_numx coeff c), pow), (poly_mult_by_numx c poly));
;;

let rec poly_div_by_numx (c : numerix) (p : poly) = match p with
  | NULL                  -> NULL;
  | P((coeff, pow), poly) -> P(((div_numx coeff c), pow), (poly_div_by_numx c poly));
;;

let poly_normalize (p : poly) =
  let cleanP = (poly_cleaning p) in
  if (cleanP <> NULL) then
    let c = (intern_lead_coeff cleanP) in
    if (eq_numx c (R(Int(1)))) then
      cleanP
    else
      poly_div_by_numx c cleanP;
  else
    NULL;
;;

let poly_minus (p : poly) = intern_poly_minus (poly_cleaning p);;

let poly_add (p1 : poly) (p2 : poly) = intern_poly_add (poly_cleaning p1) (poly_cleaning p2);;
let prefix +$ (p1 : poly) (p2 : poly) = poly_add p1 p2;;

let poly_sub (p1 : poly) (p2 : poly) = intern_poly_sub (poly_cleaning p1) (poly_cleaning p2);;
let prefix -$ (p1 : poly) (p2 : poly) = poly_sub p1 p2;;

let poly_mult (p1 : poly) (p2 : poly) = intern_poly_mult (poly_cleaning p1) (poly_cleaning p2);;
let prefix *$ (p1 : poly) (p2 : poly) = poly_mult p1 p2;;

let eucl_div (num : poly) (den : poly) = intern_eucl_div (poly_cleaning num) (poly_cleaning den);;
let prefix /$ (num : poly) (den : poly) = eucl_div num den;;

let poly_power (p : poly) (pow : int) =
  if (pow = 0) then P((R(Int(1)), 0), NULL)
  else if (pow = 1) then (poly_cleaning p)
  else if (pow > 0) then intern_poly_power (poly_cleaning p) pow
  else failwith "**** !! ERROR in 'poly_power' -- negative power !! ****";
;;
let prefix ^$ (p : poly) (pow : int) = poly_power p pow;;

let poly_derivate (p : poly) = intern_poly_derivate (poly_cleaning p);;

let poly_integrate (p : poly) = intern_poly_integrate (poly_cleaning p);;

let poly_degree (p : poly) = intern_poly_degree (poly_cleaning p);;
let lead_coeff (p : poly) = intern_lead_coeff (poly_cleaning p);;

let poly_gcd (p1 : poly) (p2 : poly) = poly_normalize (intern_poly_gcd (poly_cleaning p1) (poly_cleaning p2));;

let poly_coeff (p : poly) (pow : int) =
  let rec parsePoly p = match p with
    | P((c, p), poly) when (p > pow) -> parsePoly poly;
    | P((c, p), poly) when (p = pow) -> c;
    | _                              -> R(Int(0));
  in
  parsePoly (poly_cleaning p);
;;
(* ================================= *)

(* ================================================================================== *)

(* Square-free part of a polynomial. *)
let square_free_part (p : poly) =
  let newP = (poly_normalize p) in
  let u = (poly_gcd newP (intern_poly_derivate newP)) in
  fst(intern_eucl_div newP u);
;;

(* Yun's algorithm to compute the square-free factorization of a unitary polynomial. *)
let square_free_factor_Yun1 (p : poly) =
  (* Leading coefficient of p.*)
  let leadCoeff = lead_coeff p in
  
  (* Initialization. *)
  let newP = (poly_normalize p) in
  let newDP = (intern_poly_derivate newP) and resultList = ref [] in
  
  let CC = ref (poly_gcd newP newDP) in
  let DD = ref (fst(intern_eucl_div newP !CC)) in
  let EE = ref (intern_poly_sub (fst(intern_eucl_div newDP !CC)) (intern_poly_derivate !DD)) in
  
  (* The main loop of the procedure. *)
  let i = ref 1 in
  while (!DD <> P((R(Int(1)), 0), NULL)) do
    CC := (poly_gcd !DD !EE);
    DD := fst(intern_eucl_div !DD !CC);
    
    if (!CC <> P((R(Int(1)), 0), NULL)) then resultList := (!i, !CC)::(!resultList);
    
    if (!DD <> P((R(Int(1)), 0), NULL)) then begin
      EE := (intern_poly_sub (fst(intern_eucl_div !EE !CC)) (intern_poly_derivate !DD));
      i := !i + 1;
    end;
  done;
  
  (leadCoeff, !resultList);
;;

let square_free_factor_Yun2 (p : poly) =
  (* Leading coefficient of p.*)
  let leadCoeff = lead_coeff p in
  
  (* Initialization. *)
  let newP = (poly_normalize p) in
  let newDP = (intern_poly_derivate newP) and resultList = ref [] in
  
  let CC = ref (poly_gcd newP newDP) in
  let DD = ref (fst(intern_eucl_div newP !CC)) and EE = ref (fst(intern_eucl_div newDP !CC)) in
  
  (* The main loop of the procedure. *)
  let flag = ref true and i = ref 1 and temp = ref NULL in
  while (!flag) do
    temp := intern_poly_sub !EE (intern_poly_derivate !DD);
    CC := (poly_gcd !DD !temp);
    DD := fst(intern_eucl_div !DD !CC);
    
    if (!CC <> P((R(Int(1)), 0), NULL)) then resultList := (!i, !CC)::(!resultList);
    
    if (!DD <> P((R(Int(1)), 0), NULL)) then begin
      EE := fst(intern_eucl_div !temp !CC);
      i := !i + 1;
    end
    else
      flag := false;
  done;
  
  (leadCoeff, !resultList);
;;

(* This is the Tobey-Horowitz's algorithm. *)
(* DO NOT WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let square_free_factor_Tobey (p : poly) =
  (* Leading coefficient of p.*)
  let leadCoeff = lead_coeff p in
  
  (* Initialization. *)
  let newP = (poly_normalize p) and resultList = ref [] in
  
  let tmpC = ref newP and tmpD = ref NULL in
  let CC = ref (poly_gcd newP (intern_poly_derivate newP)) in
  let DD = ref (fst(intern_eucl_div newP !CC)) in
  
  (* The main loop of the procedure. *)
  let i = ref 1 in
  while (!CC <> P((R(Int(1)), 0), NULL)) do
    tmpC := !CC;
    tmpD := !DD;
    CC := (poly_gcd !CC (intern_poly_derivate !CC));
    DD := fst(intern_eucl_div !tmpC !CC);
    
    if (!tmpD <> !DD) then resultList := (!i, fst(intern_eucl_div !tmpD !DD))::(!resultList);
    
    i := !i + 1;
  done;
  resultList := (!i, !tmpC)::(!resultList);
  
  (leadCoeff, !resultList);
;;

let square_free_factor (p : poly) (method : int) = match method with
  | 2 -> square_free_factor_Yun2 p;
  | 3 -> square_free_factor_Tobey p;
  | _ -> square_free_factor_Yun1 p;
;;

let map_list_to_vector (l : (int * poly) list) =
  let result = (make_vect (if (l = []) then 1 else fst(hd(l))) (P((R(Int(1)), 0), NULL))) in
  let rec aux list = match list with
    | []        -> result;
    | (i, p)::q -> result.(i-1) <- p; aux q;
  in
    aux l;
;;
(* =================== *)


(* Extended Euclid's algorithm. Returns u, v, pgcd(A, B) : uA + vB = pgcd(A, B) *)
(* TODO: Use the subresultant polynomial remainder sequence ????????????????? *)
let extend_euclid (p1 : poly) (p2 : poly) =
  let (U0, V0, R0) = (ref (P((R(Int(1)), 0), NULL)), ref NULL, ref (poly_cleaning p1))
  and (U1, V1, R1) = (ref NULL, ref (P((R(Int(1)), 0), NULL)), ref (poly_cleaning p2))
  and (Quot, Reste) = (ref NULL, ref (P((R(Int(1)), 0), NULL))) in
  
  ASSERT (!R1 <> NULL) "BUG in extend_euclid : r2 is NULL !!";
  
  let cte = ref NULL in
  while (!Reste <> NULL) do
    let temp = (intern_eucl_div !R0 !R1) in
    Quot := fst(temp);
    Reste := snd(temp);
    R0 := !R1;
    R1 := !Reste;
    
    (* U2 *)
    cte := intern_poly_sub !U0 (intern_poly_mult !Quot !U1);
    U0 := !U1;
    U1 := !cte;
    
    (* V2 *)
    cte := intern_poly_sub !V0 (intern_poly_mult !Quot !V1);
    V0 := !V1;
    V1 := !cte;
  done;
  
  (* Simplify the result, according to the rest (R0). *)
  if (!R0 <> NULL) then begin
    let domC = intern_lead_coeff !R0 in
    U0 := poly_div_by_numx domC !U0;
    V0 := poly_div_by_numx domC !V0;
    R0 := poly_div_by_numx domC !R0;
  end;
  
  (* u, v, pgcd(A, B) *)
  (!U0, !V0, !R0);
;;

(* Polynomial evaluation with the Horner's method. *)
(* TODO: IT WORKS, BUT TRY TO SEARCH ANOTHER RECURSIVE METHOD. *)
let poly_eval (p : poly) (n : numerix) =
  let rec horner result p precPow = match p with
    | NULL                  -> result *% (n ^% precPow);
    | P((coeff, pow), poly) -> horner (coeff +% (result *% (n ^% (precPow - pow)))) poly pow;
  in
  
  let cleanP = poly_cleaning p in
  horner (R(Int(0))) cleanP (int_of_numerix (intern_poly_degree cleanP));
;;

(* Content (gcd of all the coefficients) of a polynomial. *)
let content (p : poly) =
  let rec parse_poly (result : numerix) (p : poly) = match p with
    | NULL                  -> result;
    | P((coeff, pow), poly) -> parse_poly (gcd_temp_numx result coeff) poly;
  in
  
  parse_poly (R(Int(0))) p;
;;

(* Primitive part of a polynomial (the polynomial divided by its content. *)
let primpart (p : poly) = match p with
  | NULL -> NULL;
  | _    -> poly_div_by_numx (content p) p;
;;

(* A polynomial factorization procedure... *)
let poly_factor (p : poly) =
  let factorList = ref [] and noFactorList = ref [] in
  let (lCoeff, sqrFreeFactor) = (square_free_factor p 1) in
  
  let rec parseList l = match l with
    | [] -> ();
    | (c, poly)::q -> begin match poly with (* poly is cleaned. *)
                        | NULL -> failwith "There is a NULL polynomial in the square-free factorization of p. It is a (big) bug!!!!!!!!";
                        | P((coeff, 0), polynom) -> failwith "There is a constant polynomial in the square-free factorization of p. It is a (big) bug, because these constant should be with the leading coefficient...";
                        | P((coeff, 1), polynom) -> factorList := (c, poly)::(!factorList);
                          (* 'polynom' is supposed to be a constant: the polynoms in the list returned by 'square_free_factor' are already sorted.*)
                        
                        | _ -> begin
                                 let simplificatedPoly = ref poly in
                                 
                                 (* Sub-function for factorization. *)
                                 let intern_factor p =
                                   factorList := (c, p)::(!factorList);
                                   
                                   let (q, r) = (!simplificatedPoly /$ p) in
                                   ASSERT (r = NULL) "**** !! HACK in 'poly_factor', 'parseList (intern_factor)' -- r <> 0 !! ****";
                                   simplificatedPoly := q;
                                 in
                                 
                                 (* Checks for rationnal roots. *)
                                 let dcg = content !simplificatedPoly in
                                 let cdP = (poly_coeff !simplificatedPoly (int_of_numerix (poly_degree !simplificatedPoly))) and vlP = (poly_coeff !simplificatedPoly 0) in
                                 let new_cdP = cdP /% dcg and new_vlP = vlP /% dcg in
                                 
                                 let new_cdP_divisors_list = divisors new_cdP in
                                 let new_vlP_divisors_list = (Int(0))::(divisors new_vlP) in
                                 
                                 let tmpVlP = ref new_vlP_divisors_list in
                                 while (!tmpVlP <> []) do
                                   let tmpCdP = ref new_cdP_divisors_list in
                                   while (!tmpCdP <> []) do
                                     let possible_root = R(hd(!tmpVlP)) /% R(hd(!tmpCdP)) in
                                     let possible_positive_real_root = possible_root in
                                     let possible_negative_real_root = neg_numx possible_positive_real_root in
                                     let possible_positive_imaginary_root = possible_positive_real_root *% C(Int(0),(Int(1))) in
                                     let possible_negative_imaginary_root = neg_numx possible_positive_imaginary_root in
                                     
                                     let polyRoot = ref NULL in
                                     
                                     (* Checks for real rationnal roots. *)
                                     if ((poly_eval !simplificatedPoly possible_positive_real_root) =% R(Int(0))) then begin
                                       if (possible_root <>% R(Int(0))) then
                                         polyRoot := P((R(Int(1)), 1), P((possible_negative_real_root, 0), NULL))
                                       else
                                         polyRoot := P((R(Int(1)), 1), NULL);
                                       
                                       intern_factor !polyRoot;
                                     end;
                                     if ((poly_eval !simplificatedPoly possible_negative_real_root) =% R(Int(0))) then begin
                                       if (possible_root <>% R(Int(0))) then
                                         polyRoot := P((R(Int(1)), 1), P((possible_positive_real_root, 0), NULL))
                                       else
                                         polyRoot := P((R(Int(1)), 1), NULL);
                                       
                                       intern_factor !polyRoot;
                                     end;
                                     
                                     (* Checks for pure imaginary rationnal roots. *)
                                     if ((poly_eval !simplificatedPoly possible_positive_imaginary_root) =% R(Int(0))) then begin
                                       if (possible_root <>% R(Int(0))) then
                                         polyRoot := P((R(Int(1)), 1), P((possible_negative_imaginary_root, 0), NULL))
                                       else
                                         polyRoot := P((R(Int(1)), 1), NULL);
                                       
                                       intern_factor !polyRoot;
                                     end;
                                     if ((poly_eval !simplificatedPoly possible_negative_imaginary_root) =% R(Int(0))) then begin
                                       if (possible_root <>% R(Int(0))) then
                                         polyRoot := P((R(Int(1)), 1), P((possible_positive_imaginary_root, 0), NULL))
                                       else
                                         polyRoot := P((R(Int(1)), 1), NULL);
                                       
                                       intern_factor !polyRoot;
                                     end;
                                     
                                     tmpCdP := tl(!tmpCdP);
                                   done;
                                   tmpVlP := tl(!tmpVlP);
                                 done;
                                 
                                 if (!simplificatedPoly <> P((R(Int(1)), 0), NULL)) then noFactorList := (c, !simplificatedPoly)::(!noFactorList);
                                 end;
                      end;
                      parseList q;
  in
  
  parseList sqrFreeFactor;
  
  (* TODO: other tests (square roots...) *)
  
  (lCoeff, !factorList, !noFactorList);
;;

let print_poly_factor ((lCoeff, factorList, noFactorList) : (numerix * (int * poly) list * (int * poly) list)) =
  let rec parseList l = match l with
    | [] -> ();
    | (c, p)::q -> print_string "("; print_poly p; print_string ")";
                   if (c > 1) then begin
                     print_string "^";
                     print_int c;
                   end;
                   if (q <> []) then begin
                     print_string " * ";
                     parseList q;
                   end;
  in
  
  if (lCoeff <>% R(Int(1))) then begin
    print_string "(";
    print_numx lCoeff;
    print_string ")";
  end;
  if (factorList <> []) then begin
    if (lCoeff <>% R(Int(1))) then print_string " * ";
    parseList factorList;
  end;
  if (noFactorList <> []) then begin
    if ((lCoeff <>% R(Int(1))) || (factorList <> [])) then print_string " * ";
    parseList noFactorList;
  end;
  
  print_newline();
;;
