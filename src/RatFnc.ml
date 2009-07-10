(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- RATFNC PACKAGE -------------------------------- Rationnal fractions manipulation.                     ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: RatFnc.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

type RatFnc = NULLR | F of (poly * (poly * poly));;

(* ========================================== *)
(* == Print rational functions Procedures. == *)
(* ========================================== *)
#open "format";;

let print_ratfnc (f : RatFnc) = match f with
  | NULLR                   -> print_string "00";
  | F(NULL, (NULL, NULL))   -> print_string "< 0* / *0 >";
  | F(intPart, (num, NULL)) -> print_poly intPart; print_string " + < 0* / *0 >";
  | F(intPart, (NULL, den)) -> print_poly intPart;
  | F(NULL, (num, den))     -> print_string "("; print_poly num; print_string ") / ("; print_poly den; print_string ")";
  | F(intPart, (num, den))  -> print_poly intPart; print_string " + [("; print_poly num; print_string ") / ("; print_poly den; print_string ")]";
;;

install_printer "print_ratfnc";;
(* ================================== *)

let ratfnc_cleaning (f : RatFnc) = match f with
  | NULLR                  -> NULLR;
  | F(intPart, (num, den)) ->
      let (quot, remaind) = (eucl_div num den) in
      begin match remaind with
        | NULL -> F((poly_add intPart quot), (NULL, NULL));
        | _    -> F((poly_add intPart quot), (remaind, den));
      end;
;;

(* Noob version to improve... *)
let ratfnc_add (f1 : RatFnc) (f2 : RatFnc) = match (f1, f2) with
  | (NULLR, NULLR)                                         -> NULLR;
  | (_, NULLR)                                             -> f1;
  | (NULLR, _)                                             -> f2;
  | (F(intPart1, (num1, den1)), F(intPart2, (num2, den2))) -> ratfnc_cleaning (F((poly_add intPart1 intPart2), ( (poly_add (poly_mult num1 den2) (poly_mult num2 den1)) , (poly_mult den1 den2))));
;;

let ratfnc_derivate (f : RatFnc) = match f with
  | NULLR                  -> NULLR;
  | F(intPart, (num, den)) ->
      let numerator = (poly_sub (poly_mult (poly_derivate num) den) (poly_mult num (poly_derivate den))) in
      begin match numerator with
        | NULL -> ratfnc_cleaning (F((poly_derivate intPart), (NULL, NULL)));
        | _    -> ratfnc_cleaning (F((poly_derivate intPart), ( numerator , (poly_mult den den))));
      end;
;;

(* deg(intPart + num/den) = deg( (intPart *$ den +$ num)/den ) = deg(intPart *$ den +$ num) - deg(den) *)
let ratfnc_degree (f : RatFnc) = match f with
  | NULLR                  -> poly_degree NULL;
  | F(intPart, (num, den)) -> sub_numx (poly_degree (poly_add (poly_mult intPart den) num)) (poly_degree den);
;;

(* ================================================================================== *)

(* Partial fraction decomposition according to the square free factorization of den (the denominator). *)
let part_frac_decompos (num : poly) (den : poly) (v : poly vect) = (* deg(num) < deg(den) *)
  let Num = (poly_cleaning (poly_div_by_numx (lead_coeff den) num)) and Den = (poly_normalize den) and m = (vect_length v) in
  let resultVector = make_vect m [|NULL|] in
  for i = 1 to m do
    resultVector.(i-1) <- (make_vect i NULL);
    
    let gi = poly_cleaning (v.(i-1)) in
    if (gi <> P((R(Int(1)), 0), NULL)) then begin
      let gii = poly_power gi i in
      let ui = fst(intern_eucl_div Den gii) in
      let (ai, bi, ri) = (extend_euclid ui gii) in
      
      (* Sanity check: ri must be equal to 1. *)
      ASSERT (ri = P((R(Int(1)), 0), NULL)) "**** !! HACK in 'part_frac_decompos' -- 'for' loop: ri <> 1 !! ****";
      
      let ri = ref (snd(intern_eucl_div (intern_poly_mult Num ai) gii)) in
      
      for j = i downto 1 do
        let temp = (intern_eucl_div !ri gi) in
        let Pij = snd(temp) in
        ri := fst(temp); (* And if ri = 0 ?? *)
        
        resultVector.(i-1).(j-1) <- Pij;
      done;
    end;
  done;
  
  resultVector;
;;

(* Hermite's reduction. *)
let HermRed (v1 : poly vect vect) (v2 : poly vect) = (* v1 is: pij/gi^j and v2 is the square_free_factorization of the denominator. *)
  let l = ref 0 in
  let rationnalPart = ref [] and logarithmicPart = ref [] in
  
  for i = 1 to (vect_length v2) do
    let gi = poly_cleaning (v2.(i-1)) in
    
    if (gi <> P((R(Int(1)), 0), NULL)) then begin
      let diffGi = (intern_poly_derivate gi) in
      let (si, ti, ri) = (extend_euclid diffGi gi) in
      
      (* Sanity check: ri must be equal to 1. *)
      ASSERT (ri = P((R(Int(1)), 0), NULL)) "**** !! HACK in 'HermRed' -- 'for' loop: ri <> 1 !! ****";
      
      for j = i downto 2 do
        let pij = poly_cleaning (v1.(i-1).(j-1)) in
        
        if (pij <> NULL) then begin
          let (q, s) = (intern_eucl_div (intern_poly_mult si pij) gi) in
          
          let t = (intern_poly_add (intern_poly_mult ti pij) (intern_poly_mult q diffGi)) in
          let delta = poly_div_by_numx (R(Int(j-1))) s in
          
          v1.(i-1).(j-2) <- (*poly_normalize*) (intern_poly_add t (intern_poly_add (intern_poly_derivate delta) (poly_cleaning (v1.(i-1).(j-2)))));
          
          l := !l + 1;
          rationnalPart := (!l, intern_poly_minus delta, poly_power gi (j-1))::(!rationnalPart);
        end;
      done;
      
      if (v1.(i-1).(0) <> NULL) then logarithmicPart := (i, v1.(i-1).(0), gi)::(!logarithmicPart);
      
    end;
  done;
  
  (!rationnalPart , !logarithmicPart);
;;

(* A logarithm / arctan / argth extractor, before calls to RothsTrag. *)
let extractor (hermRedResult : ((int * poly * poly) list * (int * poly * poly) list)) =
  let logarithmicPart = snd(hermRedResult) in
  let logList = ref [] and arctanList = ref [] and argthList = ref [] in
  
  (* The polynomials are sorted by powers. *)
  let rec parseList l = match l with
    | []             -> [];
    | (i, ai, bi)::q -> begin match (ai, bi) with
                          | (NULL, NULL) -> failwith "**** !! ERROR in 'extractor' -- two NULL polynomials found !! ****";
                          | (_, NULL)    -> failwith "**** !! ERROR in 'extractor' -- division by zero !! ****";
                          | (NULL, _)    -> parseList q;
                          | (P((coeff1, pow1), poly1), P((coeff2, pow2), poly2)) ->
                            if ((pow2 >= 2) && (pow2 = pow1 + 1)) then begin
                              print_str "--> Logarithm + log part.";
                              let coeff = (div_numx coeff1 (mult_numx (numerix_of_int pow2) coeff2)) in
                              logList := (i , coeff , bi)::(!logList);
                              let newPoly = poly_sub poly1 (poly_mult_by_numx coeff (poly_derivate poly2)) in
                              if (newPoly <> NULL) then
                                (i, newPoly, bi)::(parseList q)
                              else
                                parseList q;
                            end
                            else if ((pow1 = 0) && (pow2 = 1)) then begin
                              print_str "--> Logarithm.";
                              logList := (i , (div_numx coeff1 coeff2) , bi)::(!logList);
                              parseList q;
                            end
                            else if ((pow1 = 0) && (pow2 = 2)) then begin
                              print_str "--> Arctan or Argth.";
                              (i, ai, bi)::parseList q;
                              (* arctanList := (i , (div_numx coeff1 coeff2) , bi)::(!arctanList); *)
                              (* parseList q; *)
                            end
                            else (i, ai, bi)::parseList q;
                        end;
  in
  
  let newLogarithmicPart = parseList logarithmicPart in
  ([|fst(hermRedResult); newLogarithmicPart|] , [|!logList; !arctanList; !argthList|]);
;;


(* ====================================================== *)
(* Rothstein-Trager's algorithm for the logarithmic part. *)
(* ====================================================== *)
let RothsTrag (num : poly) (den : poly) =
  let nA = (poly_cleaning (poly_div_by_numx (lead_coeff den) num)) and nB = (poly_normalize den) in
  let dA = int_of_numerix (poly_degree nA) and dB = int_of_numerix (poly_degree nB) in
  let diffB = poly_derivate nB in
  
  (* 1- Computes the resultant R = Res(den, num -$ diffDen * 'Y' , x). *)
  print_str "Computing the resultant...";
  
  let multiA = multiPoly_from_poly nA "x"
  and multiB = multiPoly_from_poly nB "x"
  and multiDiffB = multiPoly_from_poly diffB "x" in
  let Q = multiPoly_subs multiA (multiPoly_mult multiDiffB (V("t", [Const(R(Int(1))), 1]))) in
  let Res = poly_from_monovariate_multiPoly (Resultant multiB Q "x") in
  
  (* 2- Displays a general result. *)
  print_newline();
  print_str "General result:";
  print_string "Sum( _R * ln( Gcd( "; print_poly nB;
  print_string " , ( "; print_poly nA;
  print_string " ) - ( "; print_poly diffB;
  print_string " ) * _R ) ) ,"; print_newline();
  print_string "    _R = RootOf( "; print_poly Res;
  print_string " ) )"; print_newline(); print_newline();
  
  (* 3- Tries to factorize the resultant completely in linear terms (in C[X]) *)
  print_str "Trying to factorize the resultant...";
  let (lCoeff, factorList, noFactorList) = poly_factor Res in
  (*let (lCoeff, factorList, noFactorList) = (R(Int(1)), [], [1, Res]) in*)
  let ResFactor = (lCoeff, factorList, noFactorList) in
  
  (* 4- Extract some logs. *)
  (* We search the zeroes of the Resultant and try to extract some logarithms from the factorization.. *)
  let rec parse_zeroes (l:(int * poly) list) = match l with
    | []        -> ();
    | (c, p)::q -> begin match p with
                    | P((coeff1, pow1), P((coeff2, pow2), NULL)) when ((pow1 = 1) && (pow2 = 0)) ->
                        print_string ("Res -- Log " ^ (string_of_int c) ^ " = (");
                        let Zero = (neg_numx (coeff2 /% coeff1)) in
                        print_numx Zero;
                        print_string ") * ln( ";
                        print_poly (poly_gcd nB (nA -$ (poly_mult_by_numx Zero diffB)));
                        print_string " )"; print_newline();
                    | NULL -> failwith "**** !! ERROR in 'RothsTrag' -- DO YOU SEE what I'm displaying on YOUR screen?? I've found a NULL polynomial in the factorization of a non-null Resultant!! Unthinkable !! ****";
                    | _    -> failwith "**** !! ERROR in 'RothsTrag' -- Internal error... Very bad for you !! ****";
                  end;
                  parse_zeroes q;
  in
  let rec parse_the_rest (l:(int * poly) list) = match l with
    | []        -> ();
    | (c, p)::q -> print_string "Sum( _R * ln( Gcd( "; print_poly nB;
                   print_string " , ( "; print_poly nA;
                   print_string " ) - ( "; print_poly diffB;
                   print_string " ) * _R ) ) ,"; print_newline();
                   print_string "    _R = RootOf( "; print_poly (primpart p);
                   print_string " ) )"; print_newline();
                   parse_the_rest q;
  in
  
  (* 5- Returns: Sum(logs) + Sum(logs, New resultant). *)
  print_newline();
  print_str "Simplificated result:";
  parse_zeroes factorList;
  parse_the_rest noFactorList;
  
  ();
;;

(* ================================================================================================ *)
(* ================================================================================================ *)

(* Makes screen outputs. *)
let ratfnc_integrate (f : RatFnc) =
  print_str "Integration of "; print_ratfnc f; print_newline(); print_newline();
  
  match f with
  | NULLR                                    -> print_ratfnc NULLR;
  | F(intPart, (num, den)) when (num = NULL) -> print_ratfnc (F((poly_integrate intPart), (NULL, NULL)));
  | F(intPart, (num, den))                   -> (* THE MOST IMPORTANT. *)
  
  let temp = eucl_div num den in
  let polyPart = (poly_add intPart (fst(temp))) and numerator = snd(temp) and denominator = den in
  
  if (polyPart <> NULL) then begin
    print_str "Integration of the polynomial part.";
    print_str "===================================";
    print_poly (poly_integrate polyPart); print_newline();
    print_newline();
  end;
  
  if (numerator <> NULL) then begin
    print_str "Integration of the rationnal fraction.";
    print_str "======================================";
    
    let normalizeDen = poly_normalize denominator in
    
    print_str "Computing the square-free factorization...";
    let (lCoeff, squareFreeFactor) = (square_free_factor denominator 1) in
    let sqrFactDen = map_list_to_vector squareFreeFactor and completeNum = poly_div_by_numx lCoeff numerator in
    
    print_str "Computing the partial fractions decomposition...";
    let partFrac = part_frac_decompos completeNum normalizeDen sqrFactDen in
    
    print_str "Integration of the rationnal part (Hermite)...";
    (* resultHermite = ( rationnal part , logarithmic part ) *)
    let resultHermite = HermRed partFrac sqrFactDen in
    
    print_str "Simplifying the result...";
    (* simpleResult = ( [|fst(hermRedResult); newLogarithmicPart|] , [|logarithm List; arctan List; argth List|] ) *)
    let simpleResult = extractor resultHermite in
    
    (* Internal functions. *)
    (* Rationnal part list parsing. *)
    let rec parse_rationnal_part l = match l with
      |[] -> ();
      |(i, ci, di)::q -> print_string ("c" ^ (string_of_int i) ^ " = "); print_poly ci; print_newline();
                         print_string ("d" ^ (string_of_int i) ^ " = "); print_poly di; print_newline(); print_newline();
                         parse_rationnal_part q;
    in
    (* Logarithmic part list parsing. *)
    let rec parse_logarithmic_part l = match l with
      |[] -> ();
      |(i, ai, bi)::q -> print_string ("a" ^ (string_of_int i) ^ " = "); print_poly ai; print_newline();
                         print_string ("b" ^ (string_of_int i) ^ " = "); print_poly bi; print_newline();
                         RothsTrag ai bi;
                         print_str "================";
                         parse_logarithmic_part q;
    in
    let rec parse_logarithms l = match l with
      |[] -> ();
      |(i, coeffi, logi)::q -> print_string ("Log " ^ (string_of_int i) ^ " = ("); print_numx coeffi; print_string ") * ln( "; print_poly logi; print_string " )"; print_newline();
                               parse_logarithms q;
    in
    (* let rec parse_arctans l = match l with
      |[] -> ();
      |(i, coeffi, arctani)::q -> print_string ("Arctan " ^ (string_of_int i) ^ " = ("); print_numx coeffi; print_string ") * arctan( "; print_poly arctani; print_string " )"; print_newline();
                                  parse_arctans q;
    in
    let rec parse_argths l = match l with
      |[] -> ();
      |(i, coeffi, argthi)::q -> print_string ("Argth " ^ (string_of_int i) ^ " = ("); print_numx coeffi; print_string ") * argth( "; print_poly argthi; print_string " )"; print_newline();
                                 parse_argths q;
    in *)
    
    print_newline();
    
    if ((fst(simpleResult)).(0) <> []) then begin
      print_str "Rationnal part";
      print_str "==============";
      parse_rationnal_part (fst(simpleResult)).(0);
      print_newline();
    end;
    
    if (((fst(simpleResult)).(1) <> []) || ((snd(simpleResult)).(0) <> [])) then begin
      print_str "Logarithmic part";
      print_str "================";
      
      if ((fst(simpleResult)).(1) <> []) then begin
        parse_logarithmic_part (fst(simpleResult)).(1);
        print_newline();
      end;
      if ((snd(simpleResult)).(0) <> []) then begin
        parse_logarithms (snd(simpleResult)).(0);
        print_newline();
      end;
    end;
    
    (* Not yet implemented. *)
    (* parse_arctans (snd(simpleResult)).(1);
    parse_argths (snd(simpleResult)).(2);
    print_newline(); *)
  end;
  
  print_str "Done.";
  print_newline();
;;
