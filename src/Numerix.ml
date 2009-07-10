(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- NUMERIX PACKAGE ------------------- Numbers - naturals, rationnals, reals and complex - manipulation. ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: Numerix.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

(* ============================================ *)
(* OPENING THE ARBITRARY-PRECISION LIBRARY num. *)
(* ============================================ *)
#open "arstatus";;
#open "big_int";;
#open "float";;
#open "int";;
#open "nat";;
#open "ratio";;
#open "num";;

let InitializeNumFlags ((errorWhenNullDenominator, normalizeRatio, normalizeRatioWhenPrinting, approxPrinting, floatingPrecision) : (bool * bool * bool * bool * int)) =
  (* Initialize the flags. *)
  set_error_when_null_denominator   errorWhenNullDenominator;   (* Default: true  *)
  set_normalize_ratio               normalizeRatio;             (* Default: false *)
  set_normalize_ratio_when_printing normalizeRatioWhenPrinting; (* Default: true  *)
  set_approx_printing               approxPrinting;             (* Default: false *)
  set_floating_precision            floatingPrecision;          (* Default: 12    *)
  
  (* Forces the return value is of type <unit>. *)
  ();
;;

(* HACK: DO NOT INITIALIZE THE FLAGS HERE!! *)
InitializeNumFlags (true, false, false, false, 20);;
(* ============================================ *)

(* Type NUMERIX : it is a num (nat, bigint, ratio), a complex (num * num <=> Real_part * Imaginary_part) or ±Infinity and Undefined. *)
type numerix = R of num | C of (num * num) | Infinity of bool | Undefined;;

(* ============================= *)
(* == Print numbers function. == *)
(* ============================= *)
#open "format";;

let print_numx (n : numerix) = match n with
  (* Real numbers display. *)
  | R(r)      -> begin
                   let sign = (sign_num r) in
                   
                   if (sign < 0) then
                     print_string ("- " ^ string_of_num(abs_num r))
                   else
                     print_string (string_of_num r);
                 end;
  
  (* Complex numbers display. *)
  | C(re, im) -> begin
                   let s_re = (sign_num re) and s_im = (sign_num im) in
                   
                   (* Displays the real part. *)
                   if (((s_re = 0) && (s_im = 0)) || (s_re <> 0)) then begin
                     if (s_re < 0) then
                       print_string ("- " ^ string_of_num (abs_num re))
                     else
                       print_string (string_of_num re);
                   end;
                   
                   (* Displays the imaginary part. *)
                   if (s_im <> 0) then begin
                     
                     (* Displays the sign. *)
                     if (s_im > 0) then begin
                       if (s_re <> 0) then
                         print_string " + ";
                     end
                     else if (s_im < 0) then begin
                       if (s_re <> 0) then
                         print_string " ";
                       
                       print_string "- ";
                     end;
                     
                     if not((eq_num im (Int(1))) || (eq_num im (Int(-1)))) then
                       print_string (string_of_num (abs_num im) ^ " ");
                     
                     print_string "i";
                   end;
                 end;
  
  (* Infinite or undefined cases. *)
  | Infinity(sign) -> print_string ("< " ^ (if sign then "+" else "-") ^ " Infinity >");
  | Undefined      -> print_string "< Undefined >";
;;

install_printer "print_numx";;
(* ============================= *)


(* Some constants... *)
let zero = R(Int(0));;
let one = R(Int(1));;
let i = C(Int(0), Int(1));;
let plus_infinity = Infinity(true);;
let minus_infinity = Infinity(false);;
let undefined = Undefined;;


(* ================================ *)
(* == Manipulation of Numerixes. == *)
(* ================================ *)
(* Normalization. *)
let normalize_numx (n : numerix) = match n with
  | R(r)      -> R(normalize_num r);
  | C(re, im) -> let normRe = (normalize_num re) and normIm = (normalize_num im) in
                 if (eq_num normIm (Int(0))) then R(normRe) else C(normRe, normIm);
  | _         -> n;
;;

(* The conjugate. *)
let conj (n : numerix) = match n with
  | C(re, im) -> normalize_numx (C(re, minus_num im));
  | _ (* Real case, or ±Infinity and Undefined cases. *) -> normalize_numx n;
;;

(* Real part. *)
let Re (n : numerix) = match n with
  | C(re, im) -> normalize_numx (R(re));
  | _ (* Real case, or ±Infinity and Undefined cases. *) -> normalize_numx n;
;;

(* Imaginary part. *)
let Im (n : numerix) = match n with
  | C(re, im) -> normalize_numx (R(im));
  | Undefined -> Undefined;
  | _ (* Real case, or ±Infinity cases. *) -> R(Int(0));
;;
(* ================================ *)

(* ================================ *)
(* Square root for the type num.*)
let sqrt_num (n : num) = match n with
  | Int(r)     -> normalize_num (Big_int(sqrt_big_int (big_int_of_int r)));
  | Big_int(r) -> normalize_num (Big_int(sqrt_big_int r));
  | Ratio(r)   -> normalize_num (Ratio(create_ratio (sqrt_big_int (numerator_ratio r)) (sqrt_big_int (denominator_ratio r)))); (* Is it a good approx ? *)
;;

let ceil_sqrt_num (n : num) =
  (* For time optimization. *)
  let old_normalize_flag = get_normalize_ratio () in
  set_normalize_ratio false;
  
  let result = ref (Int(0)) in
  
  begin match n with
    | r when (r >=/ Int(0)) -> result := r // Int(2);
                               let temp = ref (Int(0)) in
                               
                               while (!result <>/ !temp) do
                                 temp := !result;
                                 result := ceiling_num (!result -/ ((square_num !result) -/ (n -/ Int(1))) // (Int(2) */ !result));
                               done;
    | _ -> failwith "Error: negative number!!";
  end;
  
  set_normalize_ratio old_normalize_flag;
  normalize_num !result;
;;

(* Factorization for type num. *)
let factor_num (n : num) =
  print_string "Factorisation of "; print_num n; print_newline();
  (* For time optimization. *)
  let old_normalize_flag = get_normalize_ratio () in
  set_normalize_ratio false;
  
  let resultList = ref [] in

  begin match (normalize_num n) with
    (* Ratios are excluded. *)
    | Ratio(_)              -> ();
    
    (* Here, r is an integer: Int(_) or Big_int(_). *)
    | r when (r <>/ Int(0)) -> let a = ref (if (r >=/ Int(0)) then r else (minus_num r)) in
                               let b = ref (Int(3)) in
                               let p = ref (Int(0)) in
                               
                               let temp = ref (Int(0)) in
                               let square_root_of_a = ref (ceil_sqrt_num !a) in
                               
                               (* ============================================ *)
                               (* Cas b = 2 traité à part pour l'optimisation. *)
                               temp := !a // Int(2);
                               while (denominator_num !temp = Int(1)) do
                                 a := !temp;
                                 incr_num p;
                                 temp := !a // Int(2);
                               done;
                               
                               if (!p <>/ Int(0)) then begin
                                 resultList := (Int(2), !p)::(!resultList);
                                 p := Int(0);
                                 
                                 (* If p <> 0, then 'a' has been modified, so we recalculate its square root. *)
                                 square_root_of_a := ceil_sqrt_num !a;
                               end;
                               
                               if (Int(2) >/ !square_root_of_a) then
                                 b := normalize_num !a;
                               (* ============================================ *)
                               
                               (* ============================================ *)
                               (* Cas b > 2. *)
                               while (!a >/ Int(1)) do
                                 temp := !a // !b;
                                 while (denominator_num !temp = Int(1)) do
                                   a := !temp;
                                   incr_num p;
                                   temp := !a // !b;
                                 done;
                                 
                                 if (!p <>/ Int(0)) then begin
                                   resultList := (!b, !p)::(!resultList);
                                   p := Int(0);
                                   
                                   (* If p <> 0, then 'a' has been modified, so we recalculate its square root. *)
                                   square_root_of_a := ceil_sqrt_num !a;
                                 end;
                                 
                                 (* b := !b +/ Int(2); *) (* Not efficient. *)
                                 incr_num b;
                                 incr_num b;
                                 
                                 if (!b >/ !square_root_of_a) then
                                   b := normalize_num !a;
                               done;
                               (* ============================================ *)
    
    (* Other cases. *)
    | _ -> ();
  end;
  
  set_normalize_ratio old_normalize_flag;
  !resultList;
;;

(* Divisors for type num.*)
let divisors_num (n : num) = match (normalize_num n) with
  (* Ratios are excluded. *)
  | Ratio(_)              -> [];
  
  (* Here, r is an integer: Int(_) or Big_int(_). *)
  | r when (r <>/ Int(0)) -> let factor_list = factor_num (if (r >=/ Int(0)) then r else (minus_num r)) in
                             
                             let rec parse_factor_list l = match l with
                               | [] -> [Int(1)];
                               | m::t -> distribute (fun (x : num) (y : num) -> x */ y) (list_of_powers m) (parse_factor_list t);
                             
                             and list_of_powers (m : (num * num)) =
                               let new_pow = (if (fst(m) =/ Int(0)) then
                                                Int(1)
                                              else if (fst(m) =/ Int(1)) then
                                                Int(0)
                                              else
                                                snd(m)) in
                               let resultList = ref [] in
                               let count = ref (Int(0)) in
                               while (!count <=/ new_pow) do
                                 resultList := (power_num (fst(m)) !count)::(!resultList);
                                 incr_num count;
                               done;
                               
                               !resultList;
                             in
                             
                             parse_factor_list factor_list;  
  (* Other cases. *)
  |_ -> [];
;;
(* ================================ *)

(* ============================== *)
(* == Operations on Numerixes. == *)
(* ============================== *)
(* Addition. *)
let add_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))                     -> normalize_numx (R(add_num rx ry));
  | (R(r), C(re, im))                  -> normalize_numx (C(add_num r re, im));
  | (C(re, im), R(r))                  -> normalize_numx (C(add_num re r, im));
  | (C(rx, ix), C(ry, iy))             -> normalize_numx (C(add_num rx ry, add_num ix iy));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(_))             -> Infinity(sign);
  | (Infinity(sign), C(_))             -> Infinity(sign);
  | (R(_), Infinity(sign))             -> Infinity(sign);
  | (C(_), Infinity(sign))             -> Infinity(sign);
  | (Infinity(sign1), Infinity(sign2)) -> if ((sign1 || not(sign2)) && (not(sign1) || sign2)) then
                                            Infinity(sign1)
                                          else
                                            Undefined;
  | (Undefined, _)                     -> Undefined;
  | (_, Undefined)                     -> Undefined;
;;
let prefix +% (x : numerix) (y : numerix) = add_numx x y;;

(* Substraction. *)
let sub_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))                     -> normalize_numx (R(sub_num rx ry));
  | (R(r), C(re, im))                  -> normalize_numx (C(sub_num r re, minus_num im));
  | (C(re, im), R(r))                  -> normalize_numx (C(sub_num re r, im));
  | (C(rx, ix), C(ry, iy))             -> normalize_numx (C(sub_num rx ry, sub_num ix iy));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(_))             -> Infinity(sign);
  | (Infinity(sign), C(_))             -> Infinity(sign);
  | (R(_), Infinity(sign))             -> Infinity(not(sign));
  | (C(_), Infinity(sign))             -> Infinity(not(sign));
  | (Infinity(sign1), Infinity(sign2)) -> if ((sign1 && not(sign2)) || (not(sign1) && sign2)) then
                                            Infinity(sign1)
                                          else
                                            Undefined;
  | (Undefined, _)                     -> Undefined;
  | (_, Undefined)                     -> Undefined;
;;
let prefix -% (x : numerix) (y : numerix) = sub_numx x y;;

(* Negation. *)
let neg_numx (x : numerix) = match x with
  (* Normal cases. *)
  | R(r)           -> normalize_numx (R(minus_num r));
  | C(re, im)      -> normalize_numx (C(minus_num re, minus_num im));
  
  (* Infinite or undefined cases. *)
  | Infinity(sign) -> Infinity(not(sign));
  | Undefined      -> Undefined;
;;

(* Multiplication. *) (* TODO: DEBUG *)
let mult_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> normalize_numx (R(mult_num rx ry));
  | (R(r), C(re, im))      -> normalize_numx (C(mult_num r re, mult_num r im));
  | (C(re, im), R(r))      -> normalize_numx (C(mult_num re r, mult_num im r));
  | (C(rx, ix), C(ry, iy)) -> normalize_numx (C(sub_num (mult_num rx ry) (mult_num ix iy), add_num (mult_num rx iy) (mult_num ix ry)));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(r))             -> if (not(eq_num r (Int(0)))) then
                                            let rSign = (gt_num r (Int(0))) in
                                            Infinity((sign || not(rSign)) && (not(sign) || rSign))
                                          else
                                            Undefined;
  | (Infinity(_), C(_))                -> Undefined;
  | (R(r), Infinity(sign))             -> if (not(eq_num r (Int(0)))) then
                                            let rSign = (gt_num r (Int(0))) in
                                            Infinity((sign || not(rSign)) && (not(sign) || rSign))
                                          else
                                            Undefined;
  | (C(_), Infinity(_))                -> Undefined;
  | (Infinity(sign1), Infinity(sign2)) -> Infinity((sign1 || not(sign2)) && (not(sign1) || sign2));
  | (Undefined, _)                     -> Undefined;
  | (_, Undefined)                     -> Undefined;
;;
let prefix *% (x : numerix) (y : numerix) = mult_numx x y;;

(* Division. *) (* TODO: DEBUG *)
let div_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> begin
                                if (not(eq_num ry (Int(0)))) then
                                  normalize_numx (R(div_num rx ry))
                                else (* ry =/ Int(0) *)
                                  if (eq_num rx (Int(0))) then
                                    Undefined
                                  else
                                    Infinity(gt_num rx (Int(0)));
                              end;
  | (R(r), C(re, im))      -> let numRe = mult_num r re and numIm = minus_num (mult_num r im) in
                              let den = add_num (square_num re) (square_num im) in
                              
                              begin
                                if (not(eq_num den (Int(0)))) then
                                  normalize_numx (C(div_num numRe den, div_num numIm den))
                                else (* den =/ Int(0) *)
                                  if (eq_num r (Int(0))) then
                                    Undefined
                                  else
                                    Infinity(gt_num r (Int(0)));
                              end;
  | (C(re, im), R(r))      -> begin
                                if (not(eq_num r (Int(0)))) then
                                  if (eq_num im (Int(0))) then
                                    normalize_numx (R(div_num re r))
                                  else
                                    normalize_numx (C(div_num re r, div_num im r)) (* Even if re =/ Int(0) *)
                                else (* r =/ Int(0) *)
                                  Undefined;
                              end;
  | (C(rx, ix), C(ry, iy)) -> let numRe = add_num (mult_num rx ry) (mult_num ix iy) and numIm = sub_num (mult_num ix ry) (mult_num rx iy) in
                              let den = add_num (square_num ry) (square_num iy) in
                              
                              begin
                                if (not(eq_num den (Int(0)))) then
                                  normalize_numx (C(div_num numRe den, div_num numIm den))
                                else (* d=/ Int(0) *)
                                  Undefined;
                                  (* Since then, Infinity + x i --> Infinity ; x + Infinity i --> Undefined. *)
                              end;
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(r))     -> let rSign = (ge_num r (Int(0))) in
                                  Infinity((sign || not(rSign)) && (not(sign) || rSign));
  | (Infinity(_), C(_))        -> Undefined; (* Or not... *)
  | (R(_), Infinity(_))        -> R(Int(0));
  | (C(_), Infinity(_))        -> R(Int(0));
  | (Infinity(_), Infinity(_)) -> Undefined;
  | (Undefined, _)             -> Undefined;
  | (_, Undefined)             -> Undefined;
;;
let prefix /% (x : numerix) (y : numerix) = div_numx x y;;

(* Inversion. *)
let inv_numx (x : numerix) = div_numx (R(Int(1))) x;; (* Optimize it !!!!!!!!!! *)

(* Modulus. *)
let abs (x : numerix) = match x with
  (* Normal cases. *)
  | R(r)           -> normalize_numx (R(abs_num r));
  | C(re, im)      -> normalize_numx (R(sqrt_num (add_num (square_num re) (square_num im))));
  
  (* Infinite or undefined cases. *)
  | Infinity(sign) -> Infinity(true);
  | Undefined      -> Undefined;
;;

(* Squared modulus. *)
let abs2 (x : numerix) = match x with
  (* Normal cases. *)
  | R(r)           -> normalize_numx (R(square_num r));
  | C(re, im)      -> normalize_numx (R(add_num (square_num re) (square_num im)));
  
  (* Infinite or undefined cases. *)
  | Infinity(sign) -> Infinity(true);
  | Undefined      -> Undefined;
;;
(* let arg (x:numerix) = ;; *)
(* let exp (x:numerix) = ;; *)
(* let ln (x:numerix) = ;; *)
(* let polar_to_normal n a = ;; *)

let power_numx (n : numerix) (pow : int) = match n with
  | Infinity(sign) -> if (pow > 0) then begin
                        let signe = (pow mod 2 = 0) in
                        Infinity(sign || signe);
                      end
                      else if (pow < 0) then
                        R(Int(0))
                      else (* pow = 0 *)
                        R(Int(1));
  | Undefined      -> Undefined;
  | _ (* Real or complex numbers. *) ->
      let rec internals_power_numx (pow : int) = match pow with
        | a when (pow < 0) -> inv_numx (internals_power_numx (- a));
        | 0 -> R(Int(1));
        | 1 -> n;
        | a -> let q = internals_power_numx (a / 2) in
               if (a mod 2 = 0) then
                 (mult_numx q q)
               else
                 (mult_numx n (mult_numx q q));
      in
      internals_power_numx pow;
;;
let prefix ^% (n : numerix) (pow : int) = power_numx n pow;;

(* Gcd computation. *)
let gcd_numx (n1 : numerix) (n2 : numerix) = match (normalize_numx n1, normalize_numx n2) with
  | (R(Ratio(_)) ,R(_)) -> R(Int(1));
  | (R(_), R(Ratio(_))) -> R(Int(1));
  
  (* Case, if r1 is an Int or Big_int, and r2 too. *)
  | (R(r1), R(r2))      -> let q = ref r1
                           and r = ref r2 in
                           
                           let temp = ref (Int(0)) in
                           while (!r <>/ Int(0)) do
                             temp := !q;
                             q := !r;
                             r := normalize_num (mod_num !temp !r);
                           done;
                           
                           R(!q);
  
  (* Other cases. *)
  | (R(_), C(_)) -> R(Int(1));
  | (C(_), R(_)) -> R(Int(0));
  | _            -> R(Int(0));
;;

let gcd_temp_numx (n1 : numerix) (n2 : numerix) = match (normalize_numx n1, normalize_numx n2) with
  | (R(r1), R(r2)) -> let q = ref r1
                      and r = ref r2 in
                      
                      let temp = ref (Int(0)) in
                      while (!r <>/ Int(0)) do
                        temp := !q;
                        q := !r;
                        r := normalize_num (mod_num !temp !r);
                      done;
                      
                      R(!q);
  
  (* Other cases. *)
  | (R(_), C(_)) -> R(Int(1));
  | (C(_), R(_)) -> R(Int(0));
  | _            -> R(Int(0));
;;

(* Retrieves the numerator. *)
let numerator_numx (n : numerix) = match n with
  | R(r)           -> normalize_numx (R(numerator_num r));
  | C(re, im)      -> normalize_numx (add_numx (mult_numx (R(numerator_num re)) (R(denominator_num im))) (mult_numx (mult_numx (R(denominator_num re)) (R(numerator_num im))) (C(Int(0), Int(1)))));
  | Infinity(sign) -> Infinity(sign);
  | Undefined      -> Undefined;
;;

(* Retrieves the denominator. *)
let denominator_numx (n : numerix) = match n with
  | R(r)           -> normalize_numx (R(denominator_num r));
  | C(re, im)      -> normalize_numx (mult_numx (R(denominator_num re)) (R(denominator_num im)));
  | Infinity(sign) -> R(Int(0));
  | Undefined      -> Undefined;
;;
(* ============================== *)


(* ============================= *)
(* == Comparisons procedures. == *)
(* ============================= *)
(* Equality or difference. *)
let eq_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> eq_num rx ry;
  | (R(r), C(re, im))      -> (eq_num r re) && (eq_num im (Int(0)));
  | (C(re, im), R(r))      -> (eq_num r re) && (eq_num im (Int(0)));
  | (C(rx, ix), C(ry, iy)) -> (eq_num rx ry) && (eq_num ix iy);
  
  (* Infinite or undefined cases. *)
  | (Infinity(_), R(_)) -> false;
  | (Infinity(_), C(_)) -> false;
  | (R(_), Infinity(_)) -> false;
  | (C(_), Infinity(_)) -> false;
  | (Infinity(sign1), Infinity(sign2)) -> ((sign1 || not(sign2)) && (not(sign1) || sign2));
  | (Undefined, _)      -> false;
  | (_, Undefined)      -> false;
;;
let prefix =% (x : numerix) (y : numerix) = eq_numx x y;;
let prefix <>% (x : numerix) (y : numerix) = not(eq_numx x y);;

(* Lexical order for complexes => classical order for reals. *)
(* Less strictly. *)
let lt_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> lt_num rx ry;
  | (R(r), C(re, im))      -> ((lt_num r re) || ((eq_num r re) && (lt_num (Int(0)) im)));
  | (C(re, im), R(r))      -> ((lt_num re r) || ((eq_num re r) && (lt_num im (Int(0)))));
  | (C(rx, ix), C(ry, iy)) -> ((lt_num rx ry) || ((eq_num rx ry) && (lt_num ix iy)));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(r)) -> not(sign);
  | (Infinity(_), C(_))    -> false;
  | (R(_), Infinity(sign)) -> sign;
  | (C(_), Infinity(_))    -> false;
  | (Infinity(sign1), Infinity(sign2)) -> ((sign1 && sign2) || not(sign1));
  | (Undefined, _)         -> false;
  | (_, Undefined)         -> false;
;;

(* Less or equal. *)
let le_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> le_num rx ry;
  | (R(r), C(re, im))      -> ((lt_num r re) || ((eq_num r re) && (le_num (Int(0)) im)));
  | (C(re, im), R(r))      -> ((lt_num re r) || ((eq_num re r) && (le_num im (Int(0)))));
  | (C(rx, ix), C(ry, iy)) -> ((lt_num rx ry) || ((eq_num rx ry) && (le_num ix iy)));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(_)) -> not(sign);
  | (Infinity(_), C(_))    -> false;
  | (R(_), Infinity(sign)) -> sign;
  | (C(_), Infinity(_))    -> false;
  | (Infinity(sign1), Infinity(sign2)) -> ((sign1 && sign2) || not(sign1));
  | (Undefined, _)         -> false;
  | (_, Undefined)         -> false;
;;

(* Greater strictly. *)
let gt_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  | (R(rx), R(ry))         -> gt_num rx ry;
  | (R(r), C(re, im))      -> ((gt_num r re) || ((eq_num r re) && (gt_num (Int(0)) im)));
  | (C(re, im), R(r))      -> ((gt_num re r) || ((eq_num re r) && (gt_num im (Int(0)))));
  | (C(rx, ix), C(ry, iy)) -> ((gt_num rx ry) || ((eq_num rx ry) && (gt_num ix iy)));
  
  (* Infinite or undefined cases. *)
  | (Infinity(sign), R(_)) -> sign;
  | (Infinity(_), C(_))    -> false;
  | (R(_), Infinity(sign)) -> not(sign);
  | (C(_), Infinity(_))    -> false;
  | (Infinity(sign1), Infinity(sign2)) -> ((sign1 && sign2) || not(sign2));
  | (Undefined, _)         -> false;
  | (_, Undefined)         -> false;
;;

(* Greater or equal. *)
let ge_numx (x : numerix) (y : numerix) = match (x, y) with
  (* Normal cases. *)
  |(R(rx), R(ry))         -> ge_num rx ry;
  |(R(r), C(re, im))      -> ((gt_num r re) || ((eq_num r re) && (ge_num (Int(0)) im)));
  |(C(re, im), R(r))      -> ((gt_num re r) || ((eq_num re r) && (ge_num im (Int(0)))));
  |(C(rx, ix), C(ry, iy)) -> ((gt_num rx ry) || ((eq_num rx ry) && (ge_num ix iy)));
  
  (* Infinite or undefined cases. *)
  |(Infinity(sign), R(_)) -> sign;
  |(Infinity(_), C(_))    -> false;
  |(R(_), Infinity(sign)) -> not(sign);
  |(C(_), Infinity(_))    -> false;
  |(Infinity(sign1), Infinity(sign2)) -> ((sign1 && sign2) || not(sign2));
  |(Undefined, _)         -> false;
  |(_, Undefined)         -> false;
;;
let prefix <% (x : numerix) (y : numerix) = lt_numx x y;;
let prefix <=% (x : numerix) (y : numerix) = le_numx x y;;
let prefix >% (x : numerix) (y : numerix) = gt_numx x y;;
let prefix >=% (x : numerix) (y : numerix) = ge_numx x y;;
(* ============================= *)

(* Divisors of n. *)
let divisors (n : numerix) = match (normalize_numx n) with
  | R(r) -> divisors_num r;
  | _    -> [];
;;

(* Checks if a Numerix i a real or a pure imaginary. *)
let isReal (x : numerix) = match x with
  (* Normal cases. *)
  | R(r)        -> true;
  | C(re, im)   -> (eq_num im (Int(0)));
  
  (* Infinite or undefined cases. *)
  | Infinity(_) -> true;
  | Undefined   -> false;
;;

let isImg (x : numerix) = match x with
  (* Normal cases. *)
  | R(r)        -> false;
  | C(re, im)   -> (eq_num re (Int(0)));
  
  (* Infinite or undefined cases. *)
  | Infinity(_) -> false;
  | Undefined   -> false;
;;

(* ================================================== *)
(* == Coercions between float/int/num and numerix. == *)
(* ================================================== *)
let numerix_of_float (f : float) = normalize_numx (R(num_of_float f));;
let numerix_of_float_float ((f1, f2) : (float * float)) = normalize_numx (C(num_of_float f1, num_of_float f2));;
let numerix_of_int (i : int) = normalize_numx (R(num_of_int i));;
let numerix_of_int_int ((i1, i2) : (int * int)) = normalize_numx (C(num_of_int i1, num_of_int i2));;
let numerix_of_num (n : num) = normalize_numx (R(n));;
let numerix_of_num_num ((n1, n2) : (num * num)) = normalize_numx (C(n1, n2));;

let int_of_numerix (n : numerix) =
  if (isReal n) then
    begin match n with
      | R(r) -> int_of_num r;
      | _    -> 0;
  end
  else
    0;
;;
