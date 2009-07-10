(* ===================================================================================================================== *)
(* ==                              Symbolix Caml-CAS Version 0.0.3 Alpha Build 317                                    == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* == ---- KERNEL FILE --------------------------------- Some system and debug functions. Kernel initialization. ---- == *)
(* == --------------------------------------------------------------------------------------------------------------- == *)
(* ==                                                                                                                 == *)
(* ==           Copyright © 2008 - 2009 Hermès BÉLUSCA - MAÏTO , Lycée Pierre De Fermat. All rights reserved.         == *)
(* ===================================================================================================================== *)

(* $Id: Kernel.ml, version 0.0.3.317 2009/07/10 21:00:00 hermes $ *)

(* Opens the system library. *)
(* #open "sys";; *)

let ASSERT (cond : bool) (msg : string) =
  if not(cond) then
    if (msg = "") then failwith "ASSERTION is false."
    else failwith msg;
;;

let print_str (string : string) =
  print_string string;
  print_newline();
;;

let sleep (millisec : int) =
  let debut = sys__time() in
  while (int_of_float((sys__time() -. debut) *. 1000.) < millisec) do done;
;;

let exit () = quit();;

let CAML_CAS_Kernel_Initialize (numFlags : (bool * bool * bool * bool * int)) =
  (* Kernel initialization. *)
  print_str "Kernel is initializing...";
  sleep 1000;
  
  (* Libraries loading. *)
  print_str "Loading libraries...";
  sleep 1000;
  print_newline();
  
  include "List.ml";
  include "Numerix.ml";
  include "Polynom.ml";
  include "Multinom.ml";
  
  include "RatFnc.ml";
  
  (* Sets default num flags. *)
  (*InitializeNumFlags numFlags;*)
  
  print_newline(); print_str "Initialization complete.";
  sleep 1000;
;;
