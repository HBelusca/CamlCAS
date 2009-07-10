(* ================================================================ *)
(* ==              Caml-CAS TESTS -- INTEGRATION                 == *)
(* ================================================================ *)

(* ==========================  TEST 00  =========================== *)
print_str "TEST 00";
print_str "=======";;
let numerator = poly_of_numxList [R(Int(1)),5 ; C(Int(-3),Int(-5)),2 ; R(Int(1)),0]
and denominator = poly_of_numxList [R(Int(1)),7 ; R(Int(-1)),0];;

let f = F(NULL, (numerator, denominator));;

ratfnc_integrate f;;
(* ================================================================ *)

(* ==========================  TEST 01  =========================== *)
print_str "TEST 01";
print_str "=======";;
let numerator = poly_of_list [(1.,0)]
and denominator = poly_of_list [(1.,14);(-2.,0)];;

let f = F(NULL, (numerator, denominator));;

ratfnc_integrate f;;
(* ================================================================ *)

(* ==========================  TEST 02  =========================== *)
print_str "TEST 02";
print_str "=======";;
let numerator = poly_of_list [(8.,3);(13.,2);(5.,1);(14.,0)]
and denominator = poly_of_list [(1.,5);(3.,4);(2.,3);(6.,2);(1.,1);(3.,0)];;
(*let denominator = poly_of_numxList [(R(Int(-7)),5);(C(Int(3),Int(-2)),1);(R(Int(2)),0)];;*)

let f = ratfnc_cleaning (F(NULL, (numerator, denominator)));;

ratfnc_integrate f;;
(* ================================================================ *)

(* ==========================  TEST 03  =========================== *)
print_str "TEST 03";
print_str "=======";;
let numerator = poly_of_list [(1.,0)]
and denominator = poly_of_numxList [(R(Int(-7)),1);(C(Int(3),Int(-2)),1);(R(Int(2)),1)];;

let f = F(NULL, (numerator, denominator));;

ratfnc_integrate f;;
(* ================================================================ *)

(* ==========================  TEST 04  =========================== *)
print_str "TEST 04";
print_str "=======";;
let a = poly_cleaning (poly_of_list [(1.,11);(4.,10);(13.,9);(35.,8);(59.,7);(95.,6);(99.,5);(45.,4);(-135.,2);(-108.,1);(-108.,0)])
and b = poly_cleaning (poly_of_list [(-108.,0);(-108.,1);(432.,3);(-135.,2);(-553.,6);(65.,9);(410.,12);(-302.,15);(-35.,17);(-167.,16);(-325.,14);(50.,13);(995.,11);(470.,10);(-1171.,8);(-769.,7);(639.,5);(477.,4);(1.,23);(13.,21);(43.,19);(4.,22);(31.,20);(43.,18)]);;

ratfnc_integrate (F(NULL, (a, b)));;
(*ratfnc_integrate (F(NULL, ( P((one, 0), NULL) , b)));;*) (* TOO LONG!!!! *)
(* ================================================================ *)

(* ==========================  TEST 05  =========================== *)
(*print_str "TEST 05";
print_str "=======";;
let a = poly_of_list [(35.,5);(-6.,4);(-9.,3);(-7.,2);(51.,1);(-28.,0)]
and b = (poly_of_list [(7.,4);(3.,3);(-2.,1);(10.,0)]) ^$ 2;;

(* ?? *)
ratfnc_integrate (F(NULL, (a, b)));;
ratfnc_integrate (F(NULL, ( P((one, 0), NULL) , b)));;*)
(* ================================================================ *)

(* ==========================  TEST 06  =========================== *)
print_str "TEST 06";
print_str "=======";;
let a = poly_of_list [(-9.,3);(-7.,2);(51.,1);(-28.,0)]
and b = (poly_of_list [(2.,3);(-2.,1);(10.,0)]) ^$ 2;;

ratfnc_integrate (F(NULL, (a, b)));;
(* ================================================================ *)

(* ==========================  TEST 07  =========================== *)
print_str "TEST 07";
print_str "=======";;
let a = poly_of_list [(3.,3);(-20.,2);(197.,1);(-1966.,0)] and b = poly_of_list [(5.,4);(10.,3);(-1.,0)];;
let a = P((one, 0), NULL);;

ratfnc_integrate (F(NULL, (a, b)));;
ratfnc_integrate (F(NULL, ( P((one, 0), NULL) , b)));;
(* ================================================================ *)

(* ==========================  TEST 08  =========================== *)
print_str "TEST 08";
print_str "=======";;
let a = P((one,0),NULL)
and b = poly_of_list [(2.,9);(1.,0)];;

ratfnc_integrate (F(NULL, (a, b)));;
ratfnc_integrate (F(NULL, ( P((one, 0), NULL) , b)));;
(* ================================================================ *)

(* ==========================  TEST 09  =========================== *)
print_str "TEST 09";
print_str "=======";;
let a = poly_of_list [(2.,7);(-3.,5);(4.,4);(-6.,0)]
and b = poly_of_list [(1.,5);(30.,3);(-3.,0)];;
let a = snd(a /$ b);;

ratfnc_integrate (F(NULL, (a, b)));;
ratfnc_integrate (F(NULL, ( P((one, 0), NULL) , b)));;
(* ================================================================ *)
