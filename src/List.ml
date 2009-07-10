(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- LIST FILE -------------------------------------------------- List management (sort, distribution...). ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: List.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

(* Lists *)
#open "list";;
#open "sort";;

(* =================================== *)
(* ==     Merge-sorting lists.      == *)
(* =================================== *)
let rec cut (l : 'a list) = match l with
  | []      -> ([], []);
  | [a]     -> ([a], []);
  | a::b::t -> let (l1, l2) = (cut t) in (a::l1, b::l2);
;;

let merge_sort (merge_func : ('a list -> 'a list -> 'a list)) (l : 'a list) =
  let rec intern_merge_sort (l : 'a list) = match l with
    | []  -> l;
    | [a] -> l;
    | _   -> let (l1, l2) = (cut l) in (merge_func (intern_merge_sort l1) (intern_merge_sort l2));
  
  in
  intern_merge_sort l;
;;
(* =================================== *)

(* =================================== *)
(* ==   Distribution of 2 lists.    == *)
(* =================================== *)
(* distribute func [a;b;...;c] [d;e;...;f] = [func a d ; func a e ; ... ; func a f ; func b d ; func b e ; ... ; func b f ;
                                              ... ; func c d ;   func c e ; ... ; func c f] *)
(* TODO:  IS IT OPTIMIZED ?????????????????????????? *)
let distribute (func : ('a -> 'b -> 'c)) (l1 : 'a list) (l2 : 'b list) =
  let rec intern_distribute (l1 : 'a list) (l2 : 'b list) = match (l1, l2) with
    | ([], _)  -> [];
    | (_, [])  -> [];
    | ([a], _) -> map (func a) l2;
    | (_, [a]) -> map (func a) l1;
    | _        -> let (l11, l12) = cut l1
                  and (l21, l22) = cut l2 in
                  (intern_distribute l11 l21) @ (intern_distribute l11 l22) @ (intern_distribute l12 l21) @ (intern_distribute l12 l22);
  
  in
  intern_distribute l1 l2;
;;
(* =================================== *)

(* =================================== *)
(* ==      Merging of 2 lists.      == *)
(* =================================== *)
(*let merge_list (func : ('a -> 'a -> (bool, 'a))) (l1 : 'a list) (l2 : 'a list) =
  let rec intern_merge_list (l1 : 'a list) (l2 : 'a list) = match (l1, l2) with
    | ([], [])         -> [];
    | ([], a::t)       -> if ( ????? ) then
                            a::(intern_merge_list [] t)
                          else
                            intern_merge_list [] t;
    | (a::t, [])       -> intern_merge_list l2 l1;
    | (a1::t1, a2::t2) ->
          let result = (func a1 a2) in
          if (fst(result)) then
            (snd(result))::(intern_merge_list t1 t2)
          else
            (intern_merge_list t1 t2);
  
  in
  intern_merge_list l1 l2;
;;*)
(* =================================== *)
