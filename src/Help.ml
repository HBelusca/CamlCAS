(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- HELP FILE ------------- Help file, gives descriptions about all the packages, functions, and so on... ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: Help.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

let help () =
  (* Name and copyright. *)
  print_newline();
  print_endline "Symbolix Caml-CAS";
  print_endline "=================";
  print_endline "Version 0.0.3 Alpha Build 317";
  print_endline "Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat.";
  print_endline "All rights reserved.";
  print_newline();
  print_endline "The Caml Light system is copyright : 1989, 1990, 1991, 1992, 1993, 1994,
1995, 1996, 1997 Institut National de Recherche en Informatique et en Automatique (INRIA).";
  print_endline "INRIA holds all ownership rights to the Caml Light system.";
  print_newline();
  
  (* Software's description. *)
  print_endline "Symbolix Caml-CAS is a Computer Algebra System based on the Caml Light system, a mathematic-oriented metalanguage.";
  print_newline();
  
  (* Software's commands dexcription. *)
  print_endline "In order to start a new session, type: load();; at the Caml prompt, then press <Enter>.";
  print_endline "Type: exit();; or quit();; in order to quit Caml-CAS.";
  print_endline "Type: help();; in order to display this help screen.";
  print_newline();
  
  (* Numerix package for real and complex numbers. *)
  print_endline "Numerix package";
  print_endline "===============";
  print_endline "    Numbers - naturals, rationnals, reals and complex - manipulation.";
  print_endline "    Type NUMERIX : it is a num (nat, bigint, ratio), a complex (num * num <=> Real_part * Imaginary_part) or ±Infinity and Undefined.";
  print_endline "    Use: #open \"Numerix\".";
  print_newline();
  print_endline "    type numerix = R of num | C of (num * num) | Infinity of bool | Undefined;;";
  print_endline "    +Infinity <--> Infinity(true, false) ; -Infinity <--> Infinity(false, false).";
  print_newline();
(* TODO: add here a help line about InitializeNumFlags. *)
  print_endline "    print_numx n             - Displays a number.";
  print_endline "    normalize_numx n         - Normalizes (simplifies Ratios...) a number.";
  print_endline "    conj n                   - Returns the conjugate of a complex.";
  print_endline "    Re n                     - Returns the real part of a number.";
  print_endline "    Im n                     - Returns the imaginary part of a number.";
  print_endline "    sqrt_num n               - Returns a round-int (or ratio) of the square root of n, WHICH IS OF THE TYPE <num>.";
  print_endline "    add_numx n1 n2           - Addition (also a1 +% a2).";
  print_endline "    sub_numx n1 n2           - Substraction (also a1 -% a2).";
  print_endline "    neg_numx n               - Returns -n.";
  print_endline "    mult_numx n1 n2          - Multiplication (also: a1 *% a2).";
  print_endline "    div_numx n1 n2           - Division (also: a1 /% a2).";
  print_endline "    inv_numx n               - Inversion: 1/n.";
  print_endline "    abs_numx n               - Modulus of n.";
  print_endline "    abs2_numx n              - Squared modulus of n.";
  print_endline "    power_numx n pow         - Power (also: n ^% pow).";
  print_endline "    gcd_numx n1 n2           - Computes the gcd (greatest common divisor) of n1 and n2, if and only if n1 and n2 are reals.";
  print_endline "    numerator_numx n         - Retrieves the numerator of n.";
  print_endline "    denominator_numx n       - Retrieves the denominator of n.";
  print_endline "    eq_numx n1 n2            - Tests if n1 == n2 (also n1 =% n2). Negation: n1 <>% n2.";
  print_endline "    lt_numx n1 n2            - Lexicographical order, less strictly (also: n1 <% n2).";
  print_endline "    le_numx n1 n2            - Lexicographical order, less or equal (also: n1 <=% n2).";
  print_endline "    gt_numx n1 n2            - Lexicographical order, greater strictly (also: n1 >% n2).";
  print_endline "    ge_numx n1 n2            - Lexicographical order, greater or equal (also: n1 >=% n2).";
  print_endline "    divisors n               - Returns a sorted list of the divisors of n.";
  print_endline "    isReal n                 - Returns true if and only if n is real.";
  print_endline "    isImg n                  - Returns true if and only if n is a pure imaginary;";
  print_endline "    numerix_of_float f       - Converts a float to a Numerix-like real.";
  print_endline "    numerix_of_float_float f - Converts a 2-uplet of floats to a Numerix-like complex.";
  print_endline "    numerix_of_int i         - Converts an integer to a Numerix-like real.";
  print_endline "    numerix_of_int_int i     - Converts a 2-uplet of integers to a Numerix-like complex.";
  print_endline "    numerix_of_num n         - Converts a num to a Numerix-like real.";
  print_endline "    numerix_of_num_num n     - Converts a 2-uplet of nums to a Numerix-like complex.";
  print_endline "    int_of_numerix n         - Converts a Numerix to an integer.";
  print_newline();
  
  (* Polynomials. *)
  print_endline "Polynomials";
  print_endline "===========";
  print_endline "    Polynomials manipulation. Their coefficients are of type Numerix (coefficients on the Real or Complex field).";
  print_endline "    Use: #open \"Polynom\".";
  print_newline();
  print_endline "    type monom == (numerix * int);;";
  print_endline "    type poly = NULL | P of (monom * poly);;";
  print_endline "    The NULL polynomial represents 0.";
  print_newline();
  print_endline "    print_monom m               - Displays a monomial.";
  print_endline "    print_poly p                - Displays a polynomial.";
  print_endline "    list_of_poly p              - Creates a list of monomials.";
  print_endline "    poly_of_numxList l          - Creates a polynomial according to the list of monomials.";
  print_endline "    poly_of_list l              - Creates a polynomial according to the list of 2-uplets (float * int).";
  print_endline "    poly_cleaning p             - Sorts the monomials and delete those which are null.";
  print_endline "    poly_mult_by_numx c p       - Multiplies the polynomial p by the Numerix c.";
  print_endline "    poly_div_by_numx c p        - Divides the polynomial p by the Numerix c.";
  print_endline "    poly_normalize p            - Normalize a polynomial.";
  print_endline "    poly_minus p                - Returns -p.";
  print_endline "    poly_add p1 p2              - Addition (also: p1 +$ p2).";
  print_endline "    poly_sub p1 p2              - Substraction (also: p1 -$ p2).";
  print_endline "    poly_mult p1 p2             - Multiplication (also: p1 *$ p2).";
  print_endline "    eucl_div p1 p2              - Euclidean division of two polynomials. Returns the 2-uplet (quotient, remainder) (also: p1 /$ p2).";
  print_endline "    poly_power p pow            - Power (also: p ^$ pow).";
  print_endline "    poly_derivate p             - Derivates (differentiates) a polynomial.";
  print_endline "    poly_integrate p            - Integrates a polynomial.";
  print_endline "    poly_degree p               - Returns the degree of p (the type is Numerix. To convert into an integer, use: int_of_numerix (poly_degree p) ).";
  print_endline "    lead_coeff p                - Returns the leading coefficient of p.";
  print_endline "    poly_gcd p1 p2              - Computes the gcd (greatest common divisor) of p1 and p2.";
  print_endline "    poly_coeff p pow            - Returns the coefficient of the pow-degree monom.";
  print_endline "    square_free_part p          - Computes the square-free part of p.";
  print_endline "    square_free_factor p method - Computes the square-free factorization of p with the method:
                                  1- Yun algorithm n° 1       (direct call: square_free_factor_Yun1 p).
                                  2- Yun algorithm n° 2       (direct call: square_free_factor_Yun2 p).
                                  3- Tobey-Horowitz algorithm (direct call: square_free_factor_Tobey p).
                                  <Other choice> -> Yun algorithm n° 1.";
  print_endline "    map_list_to_vector l        - Creates a well-sized vector from the returned list of square_free_factor*.";
  print_endline "    extend_euclid p1 p2         - Computes a Bezout's relation u*p1 + v*p2 = pgcd(p1, p2) and returns (u, v, pgcd(p1, p2)).";
  print_endline "    poly_eval p n               - Evaluates the polynomial p at the point n (uses the Horner's method).";
  print_endline "    poly_factor p               - Tries to factorize the polynomial (real and pure imaginary rationnal roots).";
  print_endline "    print_poly_factor l         - Nice-displaying of the list given as a result by 'poly_factor'.";
  print_newline();
  
  (* Rationnal fractions. *)
  print_endline "Rationnal fractions";
  print_endline "===================";
  print_newline();
  
  print_endline "Online Help not yet implemented ! Sorry...";
;;
