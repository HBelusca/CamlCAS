(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- MULTINOM PACKAGE ---------------------------- Multinomials manipulation. They are recursively-stored. ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: Multinom.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

(* ===== MULTINOMIAL-TYPE DEFINITION ===== *)
type multiPoly = | Const of numerix                       (* Constant multinomials. *)
                 | V of (string * (multiPoly * int) list) (* Variable string, list of couples (multinomials * power). *)
                 | S of (multiPoly list)                  (* "Sum" of monoms. *)
                 | M of (multiPoly list);;                (* "Product" of monoms. *)

(* =================================== *)
(* == Print multinomials Functions. == *)
(* =================================== *)
#open "format";;

let rec print_multiPoly (p : multiPoly) = match p with
  | Const(n)             -> print_numx n;
  | V(var, multiPowList) -> (* Prints each coefficient. *)
      let rec parseList (l : (multiPoly * int) list) = match l with
        | [] -> ();
        | (multinom, power)::t ->
              begin match multinom with
              | Const(n) -> print_numx n;
              | _ -> print_string "("; print_multiPoly multinom; print_string ")";
              end;
              
              (* Prints the power. *)
              if ((multinom <> Const(R(Int(0)))) && (power >= 1)) then begin
                print_string " ";
                
                (* Prints the variable. *)
                print_string var;
                
                if (power >= 2) then begin
                  print_string "^";
                  print_int power;
                end;
              end;
              
              if ((t <> []) && (fst(hd(t)) <> Const(R(Int(0))))) then print_string " + ";
              parseList t;
      in
      parseList multiPowList;
  
  | S(multiList)          -> (* Prints each coefficient. *)
      let rec parseList (l : multiPoly list) = match l with
        | [] -> ();
        | multinom::t -> if (multinom <> Const(R(Int(0)))) then print_multiPoly multinom;
                         if ((multinom <> Const(R(Int(0)))) && (t <> []) && (hd(t) <> Const(R(Int(0))))) then print_string " + ";
                         parseList t;
      in
      parseList multiList;
  
  | M(multiList)          -> (* Prints each coefficient. *)
      let rec parseList (l : multiPoly list) = match l with
        | [] -> ();
        | multinom::t -> print_string "("; print_multiPoly multinom; print_string ")";
                         if (t <> []) then print_string " * ";
                         parseList t;
      in
      parseList multiList;
;;

install_printer "print_multiPoly";;
(* =================================== *)
(* =================================== *)


(* =================================== *)
(* Expanding multinomials. *)
(* TODO: Sort the monoms according to their powers?? *)
let rec expand (p : multiPoly) =
  (* Start the process. *)
  let expanded = intern_expand p in
  (* No multinomial. *)
  if (expanded = []) then
    Const(R(Int(0)))
  (* One multinomial. *)
  else if (tl(expanded) = []) then
    hd(expanded)
  (* Several multinomials. *)
  else
    S(expanded);

(* Returns: multiPoly list. *)
and intern_expand (p : multiPoly) = match p with
  | Const(c)             -> [p]; (* (instead of: [Const(c)] ; avoid a new reconstruction. *)
  | V(var, multiPowList) ->
        let rec parseList (l : (multiPoly * int) list) = match l with
          | []                   -> [];
          | (multinom, power)::t -> let expP = (intern_expand multinom) in
                                    if (power = 0) then
                                      expP @ (parseList t)
                                    else
                                      (* foreach (m) in expP do
                                           create a new list with:
                                           V(varI, [(m, power)])
                                         done;
                                         and add this to (parseList t).
                                      *)
                                      (* TODO: By induction, we prove that 'intern_expand' returns only Const(_) or V(_) list, so we
                                         can check whether m is a Const(_), or a V(_).
                                         If m = Const(c) or V(variable <> varI, _), we return V(varI, [(m, power)]).
                                         If m = V(variable, [List]) where variable = varI, then, instead of returning V(varI, [(m, power)]),
                                         we can return V(variable = varI, [List where pow <== pow + power]).
                                         If m = S(_) or M(_), we launch a failwith "...". *)
                                      (map (fun (m : multiPoly) -> V(var, [(m, power)])) expP) @ (parseList t);
        in
        parseList multiPowList;
  
  | S(multiList)      ->
        let rec parseList (l : multiPoly list) = match l with
          | []          -> [];
          | multinom::t -> (intern_expand multinom) @ (parseList t);
        in
        parseList multiList;
  
  | M(multiList)      ->
        let rec parseList (l : multiPoly list) = match l with
          | []          -> [];
          | [multinom]  -> intern_expand multinom;
          | multinom::t -> distribute (fun (m1 : multiPoly) (m2 : multiPoly) -> multiPoly_mult m1 m2) (intern_expand multinom) (parseList t);
        in
        parseList multiList;
(* =================================== *)

(* =================================== *)
(* Collecting multinomials according to one variable. *)
(* Returns only a Const(), or a V(), but not a S() nor a M() (prove it by induction). *)
and collect (p : multiPoly) (variable : string) =
  let polyList = (intern_collect p variable) in
  if (polyList = []) then
    Const(R(Int(0)))
  else if (tl(polyList) = []) then (* One and only element. *)
    let the_only_multi = hd(polyList) in
    if (snd(the_only_multi) = 0) then
      fst(the_only_multi)
    else
      V(variable, [the_only_multi]);
  else
    V(variable, polyList);

(* Returns: (multiPoly * int) list. *)
and intern_collect (p : multiPoly) (variable : string) = match p with
  | Const(c)             -> [(p, 0)]; (* (instead of: [(Const(c), 0)] ; avoid a new reconstruction. *)
  | V(var, multiPowList) ->
      let rec parseList (l : (multiPoly * int) list) = match l with
        | []                   -> [];
        | (multinom, power)::t -> let colP = (intern_collect multinom variable) in
                                  if (var = variable) then begin
                                    (* foreach (m, pow) in colP do
                                         create a new list with:
                                         (m, power + pow)
                                       done;
                                       and add this to (parseList t).
                                    *)
                                    merge1 (map (fun ((m, pow) : (multiPoly * int)) -> (*if (m <> Const(R(Int(0)))) then*) (m, power + pow))
                                                 colP) (parseList t);
                                  end
                                  else (* var <> variable *)
                                    (* foreach (m, pow) in colP do
                                         create a new list with:
                                         (V(var, [(m, power)]), pow)
                                       done;
                                       and add this to (parseList t).
                                    *)
                                    let mergedList = merge1 (map (fun ((m, pow) : (multiPoly * int)) ->
                                                 (*if (m <> Const(R(Int(0)))) then begin*)
                                                   if (power = 0) then
                                                     (m, pow)
                                                   else
                                                     (V(var, [(m, power)]), pow);
                                                 (*end*))
                                                 colP) (parseList t) in
                                    map (fun ((m, pow) : (multiPoly * int)) -> ((collect m var), pow)) mergedList;
      in
      parseList multiPowList;
  
  | S(multiList)         ->
        let rec parseList (l : multiPoly list) = match l with
          | []          -> [];
          | multinom::t -> merge1 (intern_collect multinom variable) (parseList t);
        in
        parseList multiList;
  
  | M(multiList)         ->
        (* Returns (multiPoly * int) list. *)
        let rec parseList (l : multiPoly list) = match l with
          | []          -> [];
          | [multinom]  -> intern_collect multinom variable;
          | multinom::t -> merge_sort merge1 (distribute (fun ((m1, pow1) : (multiPoly * int))
                                                              ((m2, pow2) : (multiPoly * int)) -> (multiPoly_mult m1 m2, pow1 + pow2))
                                                         (intern_collect multinom variable)
                                                         (parseList t));
        in
        parseList multiList;
(* =================================== *)

(* =================================== *)
(* Lists all the variables. *)
and listVars (p : multiPoly) = match p with
  | Const(c)             -> [];
  | V(var, multiPowList) ->
        let numberOfMulti = ref 0 in
        let rec parseList l = match l with
          | []                   -> [];
          | (Const(_), power)::t -> (* Avoid a call to listVars (Const(_)), which returns []. *)
                                    if (power <> 0) then
                                      numberOfMulti := !numberOfMulti + 1;
                                    parseList t;
          | (multinom, power)::t -> if (power <> 0) then
                                      numberOfMulti := !numberOfMulti + 1;
                                    merge0 (listVars multinom) (parseList t);
        in
        
        let proceedList = (parseList multiPowList) in
        if (!numberOfMulti <> 0) then
          merge0 [(var, !numberOfMulti)] proceedList
        else
          proceedList;
  
  | S(multiList)        ->
        let rec parseList l = match l with
          | []            -> [];
          | [Const(_)]    -> [];
          | [multinom]    -> listVars multinom;
          | (Const(_))::t -> parseList t;
          | multinom::t   -> merge0 (listVars multinom) (parseList t);
        in
        
        parseList multiList;
  
  | M(multiList)        ->
        let rec parseList l = match l with
          | []            -> [];
          | [Const(_)]    -> [];
          | [multinom]    -> listVars multinom;
          | (Const(_))::t -> parseList t;
          | multinom::t   -> merge0 (listVars multinom) (parseList t);
        in
        
        parseList multiList;

(* This version ignores couples where the second member is null. *)
and merge0 (l1 : (string * int) list) (l2 : (string * int) list) = match (l1, l2) with
  | ([], [])                                                      -> [];
  | ([], (var, num)::t)                                           -> if (num <> 0) then
                                                                       (var, num)::(merge0 [] t)
                                                                     else
                                                                       merge0 [] t;
  | ((var, num)::t, [])                                           -> merge0 l2 l1;
  | ((var1, num1)::t1, (var2, num2)::t2) when (var1 < var2)       -> if (num1 <> 0) then
                                                                       (var1, num1)::(merge0 t1 l2)
                                                                     else
                                                                       (merge0 t1 l2);
  | ((var1, num1)::t1, (var2, num2)::t2) when (var1 = var2)       -> let num = (num1 + num2) in
                                                                     if (num <> 0) then
                                                                      (var1, num)::(merge0 t1 t2)
                                                                     else
                                                                      (merge0 t1 t2);
  | ((var1, num1)::t1, (var2, num2)::t2) (* when (var1 > var2) *) -> if (num2 <> 0) then
                                                                       (var2, num2)::(merge0 l1 t2)
                                                                     else
                                                                       (merge0 l1 t2);
(* =================================== *)

(* =================================== *)
(* Addition. *)

and the_best_variable_for_collect (l1 : (string * int) list) (l2 : (string * int) list) = match (l1, l2) with
  | ([], []) -> "";
  | ([], _)  -> let rec maxList l = match l with
                  | []   -> ("", 0);
                  | [m]  -> m;
                  | m::t -> let mt = maxList t in                                 (* (a, b)::t -> let (c, d) = maxList t in            *)
                            if (snd(m) >= snd(mt)) then m else mt;                (*              if (b >= d) then (a, b) else (c, d); *)
                in
                fst(maxList l2);
                
  | (_, [])  -> the_best_variable_for_collect l2 l1; (* And calls the previous pattern-matching. *)
  | _        -> (* list 1 and list2 are sorted according to their first term. *)
                let rec intersect list1 list2 = match (list1, list2) with
                  | ([], _)                                           -> [];
                  | (_, [])                                           -> [];
                  | ((a1, b1)::t1, (a2, b2)::t2) when (a1 < a2)       -> (intersect t1 list2);
                  | ((a1, b1)::t1, (a2, b2)::t2) when (a1 = a2)       -> (a1, b1 + b2)::(intersect t1 t2);
                  | ((a1, b1)::t1, (a2, b2)::t2) (* when (a1 > a2) *) -> (intersect list1 t2);
                in
                the_best_variable_for_collect [] (intersect l1 l2);

and multiPoly_add (p1 : multiPoly) (p2 : multiPoly) = match (p1, p2) with
  | (Const(c1), Const(c2)) -> Const(c1 +% c2);
  | _                      -> let p = S[p1 ; p2] in (* Are you sure? *)
                              let varIndex = the_best_variable_for_collect [] (listVars p) in
                              collect p varIndex;

(* This version ignores couples where the second member is null. *)
(* Down-sort according to powers. *)
and merge1 (l1 : (multiPoly * int) list) (l2 : (multiPoly * int) list) = match (l1, l2) with
  | ([], [])                                                          -> [];
  | ([], (multi, pow)::t)                                             -> if (multi <> Const(R(Int(0)))) then
                                                                           (multi, pow)::(merge1 [] t)
                                                                         else
                                                                           merge1 [] t;
  | ((multi, pow)::t, [])                                             -> merge1 l2 l1;
  | ((multi1, pow1)::t1, (multi2, pow2)::t2) when (pow1 > pow2)       ->
        if (multi1 <> Const(R(Int(0)))) then
          (multi1, pow1)::(merge1 t1 l2)
        else
          (merge1 t1 l2);
  | ((multi1, pow1)::t1, (multi2, pow2)::t2) when (pow1 = pow2)       ->
        let multi = (multiPoly_add multi1 multi2) in
        if (multi <> Const(R(Int(0)))) then
          (multi, pow1)::(merge1 t1 t2)
        else
          (merge1 t1 t2);
  | ((multi1, pow1)::t1, (multi2, pow2)::t2) (* when (pow1 < pow2) *) ->
        if (multi2 <> Const(R(Int(0)))) then
          (multi2, pow2)::(merge1 l1 t2)
        else
          (merge1 l1 t2);
(* =================================== *)

(* =================================== *)
(* Substraction. *)
and multiPoly_minus (p : multiPoly) = match p with
  | Const(c)           -> Const(neg_numx c);
  | V(variable, multiPowList) ->
        let rec parseList l = match l with
          | []                   -> [];
          | (multinom, power)::t -> (multiPoly_minus multinom, power)::(parseList t);
        in
        V(variable, parseList multiPowList);
  | S(multiList)       ->
        let rec parseList l = match l with
          | []          -> [];
          | multinom::t -> (multiPoly_minus multinom)::(parseList t);
        in
        S(parseList multiList);
  | M(multiList)       -> M(Const(R(Int(-1)))::multiList);

and multiPoly_subs (p1 : multiPoly) (p2 : multiPoly) = multiPoly_add p1 (multiPoly_minus p2);
(* =================================== *)

(* =================================== *)
(* Multiplication. *)
and multiPoly_mult_by_numx (c : numerix) (p : multiPoly) =
  if (c =% R(Int(0))) then
    Const(R(Int(0)))
  else if (c =% R(Int(1))) then
    p
  else
    intern_multiPoly_mult_by_numx c p;

and multiPoly_div_by_numx (c : numerix) (p : multiPoly) =
  if (c =% R(Int(0))) then
    failwith "Error: Division by zero !"
  else if (c =% R(Int(1))) then
    p
  else
    intern_multiPoly_div_by_numx c p;

and intern_multiPoly_mult_by_numx (c : numerix) (p : multiPoly) = match p with
  | Const(const) -> Const(const *% c);
  | V(variable, multiPowList) -> let rec parseList l = match l with
                                   | [] -> [];
                                   | (multinom, power)::t -> ((intern_multiPoly_mult_by_numx c multinom), power)::(parseList t);
                                 in
                                 V(variable, parseList multiPowList);
  | S(multiList) -> let rec parseList l = match l with
                      | [] -> [];
                      | multinom::t -> (intern_multiPoly_mult_by_numx c multinom)::(parseList t);
                    in
                    S(parseList multiList);
  
  | M(multiList) -> M(Const(c)::multiList);

and intern_multiPoly_div_by_numx (c : numerix) (p : multiPoly) = match p with
  | Const(const) -> Const(const /% c);
  | V(variable, multiPowList) -> let rec parseList l = match l with
                                   | [] -> [];
                                   | (multinom, power)::t -> ((intern_multiPoly_div_by_numx c multinom), power)::(parseList t);
                                 in
                                 V(variable, parseList multiPowList);
  | S(multiList) -> let rec parseList l = match l with
                      | [] -> [];
                      | multinom::t -> (intern_multiPoly_div_by_numx c multinom)::(parseList t);
                    in
                    S(parseList multiList);
  
  | M(multiList) -> M(Const(inv_numx c)::multiList);

and multiPoly_mult (p1 : multiPoly) (p2 : multiPoly) = match (p1, p2) with
  | (Const(c1), Const(c2)) -> Const(c1 *% c2);
  | _                      -> let p = M[p1 ; p2] in (* Are you sure? *)
                              let varIndex = the_best_variable_for_collect [] (listVars p) in
                              collect p varIndex;
;;
(* =================================== *)

(* =================================== *)
(* Euclidean division. *)
let rec multiPoly_eucl_div (p1 : multiPoly) (p2 : multiPoly) = match (p1, p2) with
  | (_, Const(R(Int(0)))) -> failwith "**** !! Division by a null polynomial !! ****";
  | (Const(R(Int(0))), _) -> (p1, p1); (* Returns (0, 0). *)
  | (Const(c1), Const(c2)) -> (Const(c1 /% c2), Const(R(Int(0)))); (* Do not forget this case !! *)
  | _ -> let p = S[p1 ; p2] in
         let varIndex = the_best_variable_for_collect [] (listVars p) in
         
         let L1 = intern_collect p1 varIndex
         and L2 = intern_collect p2 varIndex in
         
         let rec do_the_job l1 l2 = match (l1, l2) with
           | ([] (* Represents Const(R(Int(0))). *), _) -> ([], []);
           | (_, []) -> failwith "**** !! Internal Error -- Division by a null polynomial !! ****";
           | ((m1, pow1)::t1, (m2, pow2)::t2) when (pow1 < pow2) -> ([], l1);
           | ((m1, pow1)::t1, (m2, pow2)::t2) (* when (pow1 >= pow2) *) ->
              let monom_quot = (multiPoly_eucl_div m1 m2) in
              
              let multiPow = (fst(monom_quot), pow1 - pow2) in
              let (quot, remaind) = (do_the_job (merge1 t1
                                                        (map (fun ((m, pow) : (multiPoly * int)) ->
                                                              (multiPoly_mult (fst(multiPow)) (multiPoly_minus m),
                                                               snd(multiPow) + pow))
                                                             t2))
                                                l2) in
              (* FIXME ?? *)
              (merge1 [multiPow] quot, merge1 [snd(monom_quot), pow1] remaind);
         in
         
         let (quotList, remaindList) = (do_the_job L1 L2) in
         
         let Quot =  (if (quotList = []) then
                        Const(R(Int(0)))
                      else if (tl(quotList) = []) then (* One and only element. *)
                        let the_only_multi = hd(quotList) in
                        if (snd(the_only_multi) = 0) then
                          fst(the_only_multi)
                        else
                          V(varIndex, [the_only_multi]);
                      else
                        V(varIndex, quotList);)
         and Remaind =  (if (remaindList = []) then
                        Const(R(Int(0)))
                      else if (tl(remaindList) = []) then (* One and only element. *)
                        let the_only_multi = hd(remaindList) in
                        if (snd(the_only_multi) = 0) then
                          fst(the_only_multi)
                        else
                          V(varIndex, [the_only_multi]);
                      else
                        V(varIndex, remaindList);)
         in
         
         (Quot, Remaind);
;;
(* =================================== *)

(* =================================== *)
(* Derivation. *)
let multiPoly_derivate (p : multiPoly) (variable : string) =
  let intern_derivate (data : multiPoly) = match data with
    | V(var, multiPowList) when (var = variable) ->
        (* Since 'data' is supposed collected, 'var' isn't in the monoms included in the list. *)
        let rec parseList l = match l with
          | [] -> [];
          | (multinom, power)::t -> if (power > 0) then
                                      (intern_multiPoly_mult_by_numx (R(Int(power))) multinom, power - 1)::(parseList t)
                                    else (* power = 0 ; the case power < 0 is a priori impossible. *)
                                      parseList t;
        in
        
        let proceedList = (parseList multiPowList) in
        if (proceedList = []) then
          Const(R(Int(0)))
        else
          V(var (* = variable *), proceedList);
    | _ (* 'intern_derivate' is an internal function, it is supposed that this function is well used. So this case is for multinomials which do not depend on the variable 'var'. *) -> Const(R(Int(0)));
  in
  
  intern_derivate (collect p variable);
;;
(* =================================== *)

(* =================================== *)
(* Integration. *)
let multiPoly_integrate (p : multiPoly) (variable : string) =
  let intern_integrate (data : multiPoly) = match data with
    | V(var, multiPowList) when (var = variable) ->
        (* Since 'data' is supposed collected, 'var' isn't in the monoms included in the list. *)
        let rec parseList l = match l with
          | [] -> [];
          | (multinom, power)::t -> if (power > 0) then
                                      (intern_multiPoly_div_by_numx (R(Int(power + 1))) multinom, power + 1)::(parseList t)
                                    else (* power = 0 ; the case power < 0 is a priori impossible. *)
                                      parseList t;
        in
        
        let proceedList = (parseList multiPowList) in
        if (proceedList = []) then
          Const(R(Int(0)))
        else
          V(var (* = variable *), proceedList);
    | _ (* 'intern_integrate' is also an internal function, it is supposed that this function is well used. So this case is for multinomials which do not depend on the variable 'var'. *) -> if (data <> Const(R(Int(0)))) then
                                                    V(variable, [(data, 1)])
                                                  else
                                                    data; (* = Const(R(Int(0))) *)
  in
  
  intern_integrate (collect p variable);
;;
(* =================================== *)

(* =================================== *)
(* Degree. *)
let degree (p : multiPoly) (variable : string) =  
  let collectedMulti = (collect p variable) in
  (* 'collectedMulti' comes from a collected multinomial; if collectedMulti = V(variable, [list]), then 'list' is already sorted. *)
  begin match collectedMulti with
    | Const(c) -> if (c <> R(Int(0))) then
                    R(Int(0))
                  else
                    Infinity(false);
    | V(var, (multinom, pow)::_) when (var = variable) -> (* Here, 'multinom' cannot be equal to Const(0). *) R(Int(pow));
    | V(_)     -> R(Int(0));
    | _        -> failwith "An error has occured during the collect process !";
  end;
;;
(* =================================== *)

(* =================================== *)
(* Leading coefficient. *)
let lcoeff (p : multiPoly) (variable : string) =  
  let collectedMulti = (collect p variable) in
  (* 'collectedMulti' comes from a collected multinomial; if collectedMulti = V(variable, [list]), then 'list' is already sorted. *)
  begin match collectedMulti with
    | Const(_) -> collectedMulti;
    | V(var, (multinom, pow)::_) when (var = variable) -> multinom;
    | V(_)     -> collectedMulti;
    | _        -> failwith "An error has occured during the collect process !";
  end;
;;
(* =================================== *)

(* =================================== *)
(* Power. *)
let rec multiPoly_power (p : multiPoly) (pow : int) =
  if (pow = 0) then
    Const(R(Int(1)))
  else if (pow = 1) then
    p
  else if (pow > 0) then
    (intern_multiPoly_power p pow)
  else
    failwith "**** !! ERROR in 'multiPoly_power' -- negative power !! ****";

and intern_multiPoly_power (p : multiPoly) (pow : int) = match pow with
  |a when (a < 0) -> failwith "**** !! ERROR in 'intern_multiPoly_power' -- negative power !! ****";
  |0 -> Const(R(Int(1)));
  |1 -> p;
  |a -> let q = (intern_multiPoly_power p (a / 2)) in
        if (a mod 2 = 0) then
          (multiPoly_mult q q)
        else
          (multiPoly_mult p (multiPoly_mult q q));
;;
(* =================================== *)

(* =================================== *)
(* Conversion poly -> multiPoly. *)
let multiPoly_from_poly (p : poly) (variable : string) =
  let rec parse_poly (p : poly) = match p with
    | NULL -> [];
    | P(m, poly) -> (Const(fst(m)), snd(m))::(parse_poly poly);
  in
  
  let list = parse_poly (poly_cleaning p) in
  (* No monom. *)
  if (list = []) then
    Const(R(Int(0)))
  (* One monom. *)
  else if (tl(list) = []) then
    let the_only_multi = hd(list) in
    if (snd(the_only_multi) = 0) then
      fst(the_only_multi)
    else
      V(variable, [the_only_multi]);
  (* Several monoms. *)
  else
    V(variable, list);
;;
(* =================================== *)

(* =================================== *)
(* Conversion multiPoly -> poly, if multiPoly contains only ONE variable. *)
let poly_from_monovariate_multiPoly (p : multiPoly) =
  (* Lists all the variables. *)
  let varList = listVars (collect p "")
  and variable = ref "" in
  
  (* Check the number of variables in the multinom. *)
  (* No variable; the multinom is a constant. Nothing to do. *)
  (* One variable. *)
  if (tl(varList) = []) then
    let the_only_var = hd(varList) in
    if (snd(the_only_var) <> 0) then
      variable := fst(the_only_var);
  (* Several variables. *)
  else
    failwith "The multinom contains more than one variable !";
  
  (* Collects the multinom. *)
  let pp = collect p !variable in
  
  (* The core function. *)
  let build_poly (data : multiPoly) = match data with
    | Const(c) -> if (c <>% R(Int(0))) then
                    P((c, 0), NULL)
                  else
                    NULL;
    | V(var, multiPowList) when (var = !variable) ->
        let rec parseList l = match l with
          | [] -> NULL;
          | (Const(c), power)::t -> if (c <>% R(Int(0))) then
                                      P((c, power), (parseList t))
                                    else
                                      parseList t;
          | _ -> failwith "BUG in build_poly::parseList !";
        in
        parseList multiPowList;
    
    (* Other cases... *)
    | _ -> failwith "BUG in build_poly !";
  in
  
  build_poly pp;
;;
(* =================================== *)

(* =================================== *)
(* Substitution function. *)
let rec subs (p : multiPoly) (variable : string) (power : int) (new_value : multiPoly) = intern_subs (collect p variable) variable power new_value;

(* We suppose that 'p' is collected according to 'varIndex'. *)
and intern_subs (p : multiPoly) (variable : string) (power : int) (new_value : multiPoly) = match p with
  | Const(_) -> p;
  | V(var, multiPowList) when (var = variable) -> (* Here, 'multiPowList' is already sorted. *)
      let rec parseList l = match l with
        | [] -> [];
        | (multinom, pow)::t when (pow = power) -> (new_value, 0)::(parseList t);
        | (multinom, pow)::t (* when (pow <> power) *) -> (multinom, pow)::(parseList t);
      in
      
      let proceedList = parseList multiPowList in
      if (proceedList = []) then
        Const(R(Int(0)))
      else
        V(var (* = variable *), proceedList);
  
  | V(_)     -> p;
  | _        -> failwith "An error has occured during the collect process !";
;;
(* =================================== *)

(* =================================== *)
(* Pseudo-remainder algorithm. *)
let prem (p : multiPoly) (q : multiPoly) (variable : string) =
  let l = ref (Const(R(Int(0))))
  and d = ref (R(Int(0)))
  and t = ref (Const(R(Int(0)))) in
  
  let v = ref (collect q variable) in
  let dv = (degree !v variable) in
  
  if (!v = Const(R(Int(0)))) then failwith "Error: division by zero !";
  
  let r = ref (collect p variable) in
  let dr = ref (degree !r variable) in
  
  if (dv <=% !dr) then begin
    l := lcoeff !v variable;
    
    if (dv =% R(Int(0))) then
      v := Const(R(Int(0)))
    else
      v := collect (intern_subs !v variable (int_of_numerix dv) (Const(R(Int(0))))) variable;
    
    d := !dr -% dv +% R(Int(1));
    
    let n = ref 0 in
    while ((dv <=% !dr) && (!r <> Const(R(Int(0))))) do
      t := collect (multiPoly_mult (multiPoly_mult (V(variable, [(Const(R(Int(1))), int_of_numerix (!dr -% dv))])) !v) (lcoeff !r variable)) variable;
      
      if (!dr =% R(Int(0))) then
        r := Const(R(Int(0)))
      else
        r := collect (intern_subs !r variable (int_of_numerix !dr) (Const(R(Int(0))))) variable;
      
      r := collect (multiPoly_subs (multiPoly_mult !l !r) !t) variable;
      dr := degree !r variable;
      
      n := !n + 1;
    done;
    
    (* "Est-ce que cela marche vraiment ???????" *)
    let power = (int_of_numerix !d) - !n in
    ASSERT (power >= 0) "prem - Negative power...";
    r := collect (multiPoly_mult (multiPoly_power !l power) !r) variable;
  end
  else
    l := Const(R(Int(1)));
  
  (* Returns (r, m = l^d). *)
  (!r, multiPoly_power !l (int_of_numerix !d));
;;
(* =================================== *)

(* =================================== *)
(* Sparse pseudo-remainder algorithm. *)
let sprem (p : multiPoly) (q : multiPoly) (variable : string) =
  let l = ref (Const(R(Int(0))))
  and d = ref (R(Int(0)))
  and t = ref (Const(R(Int(0))))
  and n = ref 0 in
  
  let v = ref (collect q variable) in
  let dv = (degree !v variable) in
  
  if (!v = Const(R(Int(0)))) then failwith "Error: division by zero !";
  
  let r = ref (collect p variable) in
  let dr = ref (degree !r variable) in
  
  if (dv <=% !dr) then begin
    l := lcoeff !v variable;
    
    if (dv =% R(Int(0))) then
      v := Const(R(Int(0)))
    else
      v := collect (intern_subs !v variable (int_of_numerix dv) (Const(R(Int(0))))) variable;
    
    d := !dr -% dv +% R(Int(1));
    
    while ((dv <=% !dr) && (!r <> Const(R(Int(0))))) do
      t := collect (multiPoly_mult (multiPoly_mult (V(variable, [(Const(R(Int(1))), int_of_numerix (!dr -% dv))])) !v) (lcoeff !r variable)) variable;
      
      if (!dr =% R(Int(0))) then
        r := Const(R(Int(0)))
      else
        r := collect (intern_subs !r variable (int_of_numerix !dr) (Const(R(Int(0))))) variable;
      
      r := collect (multiPoly_subs (multiPoly_mult !l !r) !t) variable;
      dr := degree !r variable;
      
      n := !n + 1;
    done;
  end
  else
    l := Const(R(Int(1)));
  
  (* Returns (r, m = l^n). *)
  (!r, multiPoly_power !l !n);
;;
(* =================================== *)

(* =================================== *)
(* Resultant computation. *)
let rec Resultant (p : multiPoly) (q : multiPoly) (variable : string) =
  let u = ref p in
  let du = ref (degree !u variable) in
  let v = ref q in
  let dv = ref (degree !v variable) in
  (* let c = ref (R(Int(1))) *)
  let c = ref true (* Parity: true == +1 ; false == -1. *)
  and g = ref (Const(R(Int(1))))
  and h = ref (Const(R(Int(1))))
  and r = ref (Const(R(Int(0))))
  and d = ref (R(Int(0))) in
  
  if (!du <% !dv) then
    r := Resultant q p variable
  else begin
    while (R(Int(0)) <% !dv) do
      d := !du -% !dv;
      (*c := !c *% (power_numx (R(Int(-1))) (int_of_numerix (!du *% !dv)));*)
      let aaaaah = (denominator_numx ((!du *% !dv) /% R(Int(2))) =% R(Int(0))) in (* OPTIMIZE !!!!!!!!!! *)
      c := ((!c || not(aaaaah)) && (not(!c) || aaaaah));
      r := fst(prem !u !v variable);
      u := !v;
      v := !r;
      du := !dv;
      dv := degree !r variable;
      
      if (R(Int(0)) <% !d) then begin
        let division = multiPoly_eucl_div !v (multiPoly_mult !g (multiPoly_power !h (int_of_numerix !d))) in
        
        (* Sanity check: the remainder must be equal to 0. *)
        (*print_multiPoly (snd(division)); print_newline();*)
        ASSERT (snd(division) = Const(R(Int(0)))) "Bug: division 1 failed !";
        
        v := fst(division);
      end;
      
      g := lcoeff !u variable;
      
      if (R(Int(0)) <% !d) then begin
        let power_d = int_of_numerix !d in
        let division = multiPoly_eucl_div (multiPoly_power !g power_d) (multiPoly_power !h (power_d - 1)) in
        
        (* Sanity check: the remainder must be equal to 0. *)
        (*print_multiPoly (snd(division)); print_newline();*)
        ASSERT (snd(division) = Const(R(Int(0)))) "Bug: division 2 failed !";
        
        h := fst(division);
      end;
    done;
    
    if (!du =% R(Int(1))) then
      (* r := intern_multiPoly_mult_by_numx !c !v *)
      r := (if !c then !v else (multiPoly_minus !v))
    else begin
      let power_du = int_of_numerix !du in
      let division = multiPoly_eucl_div (multiPoly_power !v power_du) (multiPoly_power !h (power_du - 1)) in
      
      (* Sanity check: the remainder must be equal to 0. *)
      (*print_multiPoly (snd(division)); print_newline();*)
      ASSERT (snd(division) = Const(R(Int(0)))) "Bug: division 3 failed !";
      
      (* r := intern_multiPoly_mult_by_numx !c (fst(division)); *)
      r := (if !c then (fst(division)) else (multiPoly_minus (fst(division))));
    end;
  end;
  
  !r;
;;
(* =================================== *)
