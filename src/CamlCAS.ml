(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- MAIN FILE ------------------------------------------ Main file, loads all the packages and functions. ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: CamlCAS.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

include "Kernel.ml";;
include "Help.ml";;



let load () =
  
  let splash_screen =
"



               SSSSSS     YY    YY     MM    MM     BBBBBB      OOOOOO     LL          IIIIIIII    XX    XX
              SS    SS    YY    YY     MMM  MMM     B     B    OO    OO    LL             II        X    X
              SS           YY  YY      M  MM  M     B     B    OO    OO    LL             II         X  X
               SSSSSS        YY        M      M     BBBBBB     OO    OO    LL             II          XX
                    SS       YY        M      M     B     B    OO    OO    LL             II         X  X
              SS    SS       YY        M      M     B     B    OO    OO    LL             II        X    X
               SSSSSS       YYYY      MMM    MMM    BBBBBB      OOOOOO     LLLLLLLL    IIIIIIII    XX    XX
                
                
                
                
               CCCCC         A         MM    MM     LL                      CCCCCC         A         SSSSSS
              CC   CC       A A        MMM  MMM     LL                     CC    CC       A A       SS    SS
              CC           A   A       M  MM  M     LL        ~~~          CC            A   A      SS
              CC           AAAAA       M      M     LL       ~~  ~~~  ~~   CC            AAAAA       SSSSSS
              CC           A   A       M      M     LL            ~~~      CC            A   A            SS
              CC   CC     A     A      M      M     LL                     CC    CC     A     A     SS    SS
               CCCCC     AAA   AAA    MMM    MMM    LLLLLLLL                CCCCCC     AAA   AAA     SSSSSS
                
                
                
                                              Version 0.0.3 Alpha Build 317
                                              
                          Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat.
                                                  All rights reserved.
                                                  
                The Caml Light system is copyright : 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997
                        Institut National de Recherche en Informatique et en Automatique (INRIA).
                                INRIA holds all ownership rights to the Caml Light system.



"
  in
  
  print_str splash_screen;
  print_str "Loading...";
  
  (* Initializes the Kernel. *)
  CAML_CAS_Kernel_Initialize (true, false, false, false, 20);
  
  (* All is done !! *)
  print_str "Done.";
;;
