let multinome1 =
V("t", [
(V("x", [
       (Const(R(Int(5))), 1) ; 
       (V("y", [(Const(R(Int(3))), 1)]), 2) ; 
       (Const(R(Int(-7))), 0)
     ]), 4)
;

(V("x", [(Const(R(Int(28))), 3)]), 2)
]);;

let multinome2 =
V("y", [
(V("t", [
       (Const(R(Int(987))), 2) ; 
       (V("y", [(Const(R(Int(3))), 1)]), 2) ; 
       (Const(R(Int(-7))), 3)
     ]), 4)
;

(V("y", [(Const(R(Int(-28))), 8)]), 2)
]);;

let multinome3 =
V("y", [
(S[V("t", [
       (Const(R(Int(987))), 2) ; 
       (V("y", [(Const(R(Int(3))), 1)]), 2)
     ]) ; V("t", [Const(R(Int(-7))),3])], 4)
;

(V("y", [(Const(R(Int(-28))), 8)]), 2)
]);;

multinome1;;
collect multinome1 "x";;
collect multinome1 "y";;
collect multinome1 "z";;
collect multinome1 "t";;

multinome2;;
collect multinome2 "x";;
collect multinome2 "y";;
collect multinome2 "z";;
collect multinome2 "t";;

let essai = V("t", [Const(R(Int(5))),3 ; Const(R(Int(2))),0]);;
collect essai "t";;
let aaa = multiPoly_subs essai (V("t", [Const(R(Int(2))),0]));;
collect aaa "t";;

let essai = S[V("t", [Const(R(Int(5))),4]) ; V("t", [V("t", [Const(R(Int(5))),1 ; V("x", [Const(R(Int(7))),2]),0]),3])];;
expand essai;;



let essai = V("x", [V("x", [Const(R(Int(785))),3]),5 ; V("t", [Const(R(Int(-7))),7]),2]);;
collect essai "sdfg qkl";;


(* ================== *)
let aaa = V("y", [Const(R(Int(-28))),8]);;
collect aaa "z";;
collect aaa "y";;
intern_collect aaa "y";;
intern_collect aaa "z";;


(* ===== OK ========= *)
let aaa = V("x", [V("x", [Const(R(Int(7))),2]),5 ; V("t", [Const(R(Int(28))),11]),2 ; V("y", [V("x", [Const(R(Int(-37))),2]),8 ; Const(R(Int(7))),5]),3]);;
collect aaa "t";;

(* ====== OK ======== *)
let multinome4 = M[multinome1 ; multinome3];;
collect multinome4 "y";;
expand multinome4;;
multiPoly_subs (expand multinome4) multinome4;;


let mxx = V("t", [(V("x", [(Const(R(Int(5))), 1) ; (V("y", [(Const(R(Int(3))), 1)]), 2)]), 4) ; (V("x", [(Const(R(Int(28))), 3)]), 2) ; (V("x", [(Const(R(Int(-28))), 0)]), 2)]);;
let essai = multiPoly_subs multinome1 mxx;;
poly_from_monovariate_multiPoly essai;;





(* ====== OK ======== *)
let essai1 = multiPoly_eucl_div (multiPoly_add multinome1 multinome2) multinome1;;
let essai2 = multinome2;;
fst(essai1);;
snd(essai1);;
essai2;;




let a = multiPoly_add (V("x", [Const(R(Int(1))),3])) (V("y", [Const(R(Int(-1))),3]));;
let b = multiPoly_add (V("x", [Const(R(Int(1))),1])) (V("y", [Const(R(Int(-1))),1]));;
let c = V("x", [Const(R(Int(1))),4 ; V("y", [Const(R(Int(1))),1 ; Const(R(Int(5))),0]),3 ; V("y", [Const(R(Int(-1))),3]),1 ; V("y", [Const(R(Int(-1))),4 ; Const(R(Int(-5))),3]),0]);;
let division = multiPoly_eucl_div c b;;
fst(division);;
snd(division);;
